'use strict';

/**
 * 
 * @param {ArrayBuffer} arrayBuffer
 * @returns {Promise<AudioBuffer>}
 */
export const array2AudioBufferImpl = (arrayBuffer) => {

  const ctx = new (window.AudioContext || window.webkitAudioContext)();
  return ctx.decodeAudioData(arrayBuffer).finally(() => ctx.close());
};
