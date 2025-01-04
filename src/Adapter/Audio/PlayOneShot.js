"use strict";

export const playOneShotImpl = (volume, buffer) => async () => {
  const ctx = new (window.AudioContext || window.webkitAudioContext)();
  const sourceNode = ctx.createBufferSource();
  const gainNode = ctx.createGain();

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
