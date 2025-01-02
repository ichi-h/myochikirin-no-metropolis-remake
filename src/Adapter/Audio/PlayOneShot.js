"use strict";

/**
 * @type {Map<string, AudioBuffer>}
 */
const cachedBufferMap = new Map();

export const playOneShotImpl = (volume, source) => async () => {
  const ctx = new (window.AudioContext || window.webkitAudioContext)();
  const sourceNode = ctx.createBufferSource();
  const gainNode = ctx.createGain();

  const hash = await crypto.subtle.digest('SHA-1', source);
  const hashHex = Array.from(new Uint8Array(hash)).map(b => b.toString(16).padStart(2, '0')).join('');
  let buffer = null;
  if (cachedBufferMap.has(hashHex)) {
    buffer = cachedBufferMap.get(hashHex);
  } else {
    buffer = await ctx.decodeAudioData(source);
    cachedBufferMap.set(hashHex, buffer);
  }

  sourceNode.buffer = buffer;
  sourceNode.connect(gainNode);
  gainNode.connect(ctx.destination);
  gainNode.gain.value = volume;
  sourceNode.start();
  sourceNode.onended = () => {
    sourceNode.disconnect();
    gainNode.disconnect();
    ctx.close();
  };
};
