# Vite Elm tailwindcss Template

A default template for building Elm applications using Vite with TailwindCSS. Includes hot-module reload of Elm modules (courtesy of `vite-plugin-elm`).

> Vite (French word for "fast", pronounced /vit/) is a build tool that aims to provide a faster and leaner development experience for modern web projects.

> Elm is a functional language that compiles to JavaScript. It helps you make websites and web apps. It has a strong emphasis on simplicity and quality tooling.

> tailwindcss is a utility-first CSS framework for rapidly building custom user interfaces.

## Get Started

```bash
# Clone the template locally, removing the template's Git log
npx degit nanahs/vite-elm-tailwindcss-template my-elm-app

# Enter the project, install dependencies, and get started!
cd my-elm-app
npm install
npm run dev
```

For more information about Vite, check out [Vite's official documentation.](https://vitejs.dev/)

To learn more about Elm, check out [Elm's official homepage](https://elm-lang.org/).

To learn more about tailwindcss, check out [tailwindcss's official homepage](https://tailwindcss.com/).

## Nice to haves

- Wrap whole svg viewbox with an element that has resize enabled and use [ResizeObserver](https://developer.mozilla.org/en-US/docs/Web/API/Resize_Observer_API) to get bring the new height/width of the box back into elm
- Convert box from rect to a List of Lines. Should allow for different shapes
- Mark surfaces as reflective or not and make the ray use the property

## Current issues

- Improper segment intersection calculation. This was a big stopper for me and sank a majority of my time into it
- The current app does not give you an idea of the total number of reflections that are possible

## Demo

https://adoring-lumiere-95dd98.netlify.app/
