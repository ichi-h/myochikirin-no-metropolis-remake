/** @type {import('tailwindcss').Config} */
module.exports = {
  content: [
    '*.css',
    'index.{html,js}',
    'src/**/*.purs',
  ],
  theme: {
    screens: {
      'xs': '375px',
      'sm': '640px',
      'md': '768px',
      'lg': '1024px',
      'xl': '1280px',
      '2xl': '2560px',
    },
    'backgroundImage': {
      'top': 'url(/images/top.webp)',
      'title': 'url(/images/title.webp)',
      'home': 'url(/images/home.webp)',
      'secret': 'url(/images/secret.webp)',
      '1': 'url(/images/1.webp)',
      '2': 'url(/images/2.webp)',
      '3': 'url(/images/3.webp)',
      '4': 'url(/images/4.webp)',
      '5': 'url(/images/5.webp)',
      '6': 'url(/images/6.webp)',
      '7': 'url(/images/7.webp)',
    },
    extend: {
      colors: {
        'primary': '#0A0A05',
        'secondary': '#FBFAF5',
      },
      width: {
        /** 512px */
        '128': '32rem',
        /** 640px */
        '160': '40rem',
        /** 768px */
        '192': '48rem',
        /** 896px */
        '224': '56rem',
        /** 1024px */
        '256': '64rem',
        /** 1152px */
        '288': '72rem',
        /** 1280px */
        '320': '80rem',
      },
      height: {
        '1/12': '8.333333%',
        '128': '32rem',
        '160': '40rem',
        '192': '48rem',
        '224': '56rem',
      },
      fontFamily: {
        'pigmo': ['pigmo'],
      },
      transitionDuration: {
        '2000': '2000ms',
      },
      animation: {
        blinking: 'blinking 3s linear infinite',
      },
      keyframes: {
        blinking: {
          '0%, 100%': { opacity: 0 },
          '50%': { opacity: 1 }
        },
      }
    }
  },
  variants: {},
  plugins: []
}
