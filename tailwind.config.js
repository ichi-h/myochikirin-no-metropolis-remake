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
      'top': 'url(/assets/images/top.webp)',
      'title': 'url(/assets/images/title.webp)',
      'home': 'url(/assets/images/home.webp)',
      'secret': 'url(/assets/images/secret.webp)',
      '1': 'url(/assets/images/1.webp)',
      '2': 'url(/assets/images/2.webp)',
      '3': 'url(/assets/images/3.webp)',
      '4': 'url(/assets/images/4.webp)',
      '5': 'url(/assets/images/5.webp)',
      '6': 'url(/assets/images/6.webp)',
      '7': 'url(/assets/images/7.webp)',
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
