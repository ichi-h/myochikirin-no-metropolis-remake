"use strict";

/**
 * @type {Map<string, {
 *   ctx: AudioContext,
 *   sourceNode: AudioBufferSourceNode,
 *   gainNode: GainNode,
 *   volume: number,
 * }>}
 */
const channelMap = new Map();

const channelNotFound = (id) => `Channel "${id}" not found`;

/**
 * @param {GainNode} gainNode
 * @param {number} targetVolume
 * @param {number} startTime
 * @param {number} fadeInMs
 * @returns {void}
 */
const fadeIn = (gainNode, targetVolume, startTime, fadeInMs) => {
  gainNode.gain.setValueAtTime(0, startTime);
  gainNode.gain.linearRampToValueAtTime(targetVolume, startTime + fadeInMs / 1000);
};

/**
 * @param {GainNode} gainNode
 * @param {number} currentVolume
 * @param {number} startTime
 * @param {number} fadeOutMs
 * @returns {void}
 */
const fadeOut = (gainNode, currentVolume, startTime, fadeOutMs) => {
  gainNode.gain.cancelScheduledValues(startTime);
  gainNode.gain.setValueAtTime(currentVolume, startTime);
  gainNode.gain.linearRampToValueAtTime(0, startTime + fadeOutMs / 1000);
};

/** 
 * @param {string} id
 * @param {ArrayBuffer} source
 * @param {number} volume
 * @param {boolean} isLoop
 * @param {number} loopStart
 * @param {number} loopEnd
 * @returns {Promise<void>}
 */
export const registerImpl = (id, source, volume, isLoop, loopStart, loopEnd) => async () => {
  const channel = channelMap.get(id);
  if (channel !== undefined) {
    channel.sourceNode.stop();
    channel.sourceNode.disconnect();
    channel.gainNode.disconnect();
    channel.ctx.close();
  }

  const ctx = new (window.AudioContext || window.webkitAudioContext)();
  const sourceNode = ctx.createBufferSource();
  const gainNode = ctx.createGain();

  const decodedData = await ctx.decodeAudioData(source);
  const sampleRate = decodedData.sampleRate;
  sourceNode.buffer = decodedData;
  sourceNode.loop = isLoop;
  sourceNode.loopStart = loopStart / sampleRate;
  sourceNode.loopEnd = loopEnd / sampleRate;

  sourceNode.connect(gainNode);
  gainNode.connect(ctx.destination);

  channelMap.set(id, {
    ctx,
    sourceNode,
    gainNode,
    playStatus: "Standby",
    volume,
  });
};

/** 
 * @param {string} id
 * @param {number} delayMs
 * @param {number} offsetMs
 * @param {number} fadeInMs
 * @param {number} fadeOutMs
 * @returns {string}
 */
export const playImpl = (id, delayMs, offsetMs, fadeInMs, fadeOutMs) => () => {
  const channel = channelMap.get(id);
  if (channel === undefined) {
    return channelNotFound(id);
  }

  const { sourceNode, gainNode, volume } = channel;

  try {
    sourceNode.start(delayMs / 1000, offsetMs / 1000);
  } catch (error) {
    return error.message;
  }
  fadeIn(gainNode, volume, 0, fadeInMs);

  if (!sourceNode.loop) {
    const duration = sourceNode.buffer.duration;
    fadeOut(gainNode, volume, duration - fadeOutMs / 1000, fadeOutMs);
  }

  return "";
};

/** 
 * @param {string} id
 * @param {number} fadeOutMs
 * @returns {string}
 */
export const stopImpl = (id, fadeOutMs) => () => {
  const channel = channelMap.get(id);
  if (channel === undefined) {
    return channelNotFound(id);
  }

  const { ctx: { currentTime }, sourceNode, gainNode, volume } = channel;

  fadeOut(gainNode, volume, currentTime, fadeOutMs);
  setTimeout(() => sourceNode.stop(), fadeOutMs);

  return "";
};

/** 
 * @param {string} id
 * @param {number} fadeOutMs
 * @returns {string}
 */
export const pauseImpl = (id, fadeOutMs) => () => {
  const channel = channelMap.get(id);
  if (channel === undefined) {
    return channelNotFound(id);
  }

  const { ctx, gainNode, volume } = channel;
  const { currentTime } = ctx;

  fadeOut(gainNode, volume, currentTime, fadeOutMs);
  setTimeout(() => ctx.suspend(), fadeOutMs);

  return "";
};

/** 
 * @param {string} id
 * @param {number} fadeInMs
 * @returns {string}
 */
export const resumeImpl = (id, fadeInMs) => () => {
  const channel = channelMap.get(id);
  if (channel === undefined) {
    return channelNotFound(id);
  }

  const { ctx, gainNode, volume } = channel;
  const { currentTime } = ctx;

  ctx.resume();
  fadeIn(gainNode, volume, currentTime, fadeInMs);

  return "";
};

/** 
 * @param {string} id
 * @param {number} volume
 * @returns {string}
 */
export const changeVolumeImpl = (id, volume) => () => {
  const channel = channelMap.get(id);
  if (channel === undefined) {
    return channelNotFound(id);
  }

  const { gainNode } = channel;
  gainNode.gain.setValueAtTime(volume, 0);
  channelMap.set(id, { ...channel, volume });

  return "";
};
