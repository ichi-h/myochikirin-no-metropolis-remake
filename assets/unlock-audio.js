(() => {
  document.addEventListener('click', async function handleClick() {
    document.removeEventListener('click', handleClick);

    const AudioCtx = window.AudioContext || window.webkitAudioContext;
    const ctx = new AudioCtx();

    await (ctx.state === 'suspended' ? ctx.resume() : Promise.resolve());

    const osc = ctx.createOscillator();
    const gain = ctx.createGain();

    osc.type = 'sine';
    osc.frequency.value = 440;
    gain.gain.value = 0;

    osc.connect(gain).connect(ctx.destination);
    osc.start();
    osc.stop(0.1);
  });
})();
