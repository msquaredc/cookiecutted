# elm-spa-boilerplate

## ----- WORK IN PROGRESS -----

This project is a Work in Progress. It is not stable and subject to change at any time.

## Introduction

A simple, no-frills boilerplate for creating create delightful Single Page Applications (SPAs) in Elm. Created for Elm 0.19. [Check out the demo!](https://elm-spa-boilerplate.netlify.com/)

elm-spa-boilerplate has everything you need to get started with no extra clutter. Just clone, compile and get right to coding.

### Highlights

* Client-side routing that uses [pushState navigation](https://developer.mozilla.org/en-US/docs/Web/API/History_API) and the forward slash `/` as the path separator. Handle 404 pages however you'd like. Optionally, use hash-based routing if required.
* Support for localStorage, with the required Elm ports and JS handlers already initialized.
* Search Engine Optimization (SEO) friendly - unique title for each page.
* File structure that follows and enforces good coding practices in Elm.
* Support for responsive websites. Webpack ready.
* Well-commented code!

## Getting Started

These instructions will walk you through getting the project up and running on your local system for development and testing.

### Prerequisites

First off, you should clone the repo! You'll also need [Node.js 8.0+][nodejs].

Of course, you'll need Elm - if you don't have Elm yet, follow the [installation instructions here][elm install], or run `npm install -g elm`. That being said, if you're completely new to Elm, you should probably check out the [Elm guide][Elm guide] before using this boilerplate!

`elm-spa-boilerplate` is bootstrapped with [create-elm-app][create elm app] for development and production. I've found that `create-elm-app` is the only Elm development server that fully supports SPAs without any additional configuration.

Install `create-elm-app` by running

```none
npm install -g create-elm-app
```

### Development Build

From the root project directory, run

```none
elm-app start
```

Then, go to [http://localhost:3000](http://localhost:3000) to see the site in action!

As you make changes in your code, the site will be reloaded automatically so you can see your changes in real time.

_Note 1_: If you don't want to use `create-elm-app`, [read these instructions](#alternatives-to-create-elm-app).

_Note 2_: `elm reactor` will **_not_** work for SPAs!

_Note 3_: I've found that you may occasionally get a JS error when using the live reload with create-elm-app. I believe this has to do with an interaction between SPAs and hot module replacement - rest assured, it will not happen in production!

### Production Build

Build minified, bundled, production-ready files by running

```none
elm-app build
```

This will create the `build/` folder with production ready files that can be deployed however you'd like!

## Usage

Some guidance on configuring and using the boilerplate.

### Adding and Removing Pages

I have included commented code that demonstrates how to add a new page. I would recommend leaving these comments there so you don't forget what modifications you need to make every time you need to add/remove a page. Here's how to add a new page:

* In the `src/Page` folder, create a new file named `<NameOfPage>.elm`. Copy and paste the code from [src/NewPage.elm](src/NewPage.elm) into it, and rename the module (line 1) appropriately.
* Search _all_ Elm files for "newPage". For each search result, copy and paste the commented line, and replace the text "newPage" with the name of your page.

Done! You've added a page. Removing a page is the same process, but instead of adding files/lines, delete them!

Once you get familiar with the boilerplate, you can uncomment these "guidance" comments if you'd like, but I still recommend keeping them there as it could save you a lot of hassle.

### Routing with Hashes (Fragments)

If you need to route with hashes for some reason (maybe your production server doesn't support 404 redirection, or nested URls), I have included the necessary code to do so. Just search [Main.elm](src/Main.elm) for "hash-based routing", uncomment the code that follows the comment, and comment out the existing code.

Hash-based routing is also one of solutions to using your own development server if you don't want to use create-elm-app. [See the section below](#alternatives-to-create-elm-app) for details.

### JavaScript Interop (Ports)

Add your JavaScript handlers in [src/index.js](src/index.js) (or create a new file if you'd like). Define Elm ports in [src/Ports.elm](src/Ports.elm), and import this file anywhere you need to use a Port. Note that the file structure is designed so that Main.elm handles all Ports, and will pass messages to the active page as needed. Using JS interop for localStorage is already set up - see the section below!

Read more about JavaScript interop with ports in Elm [here][elm ports].

### LocalStorage

The necessary initialization for using localStorage is implemented.  There are three ports:

* `onLocalStorageChange`: A listener for any changes in localStorage
* `toLocalStorage`: Write from Elm to localStorage
* `clearLocalStorage`: Convenient port for clearing localStorage

The corresponding handlers for these ports can be found in [index.js](src/index.js). You can also change the key you'd like to use by changing `storageName` in [index.js](src/index.js).

The structure of the localStorage object is found in [Type.LocalStorage.elm](src/Type/LocalStorage.elm) - change this code as needed to match the structure of the data you'd like. On initialization, the data in localStorage will be sent to Elm through a flag, and stored in the `Session` if it is of a valid structure.

A demonstration of how to interact with localStorage on the [Home page](src/Top.elm) - reading through this code should make it pretty clear how to use localStorage!

### Responsive Pages

Create [responsive webpages](https://www.w3schools.com/html/html_responsive.asp) based on the width/height of the page by using the `Session.WindowSize` value. This value is initialized through a flag and will be updated any time the window size changes.

### Configuration

For most of your configuration needs, like adding images, setting up environment variables, using a CSS Preprocessor or elm-css, follow the instructions in the [create-elm-app docs][create elm app docs].

A special note on service workers - service workers are used for creating [Progressive Web Apps](https://developers.google.com/web/progressive-web-apps/). They are used in production only. If you don't need this, remove the call to `serviceWorkerRegistration.register` from [src/index.js](src/index.js), and feel free to remove [registerServiceWorker.js](src/registerServiceWorker.js). Read more about the service worker implementation in create-elm-app [here.](https://github.com/halfzebra/create-elm-app/blob/master/template/README.md#making-a-progressive-web-app)

### Cleaning the Repo

I aimed to have as little clutter and dependencies as possible in this repo, but there were some files I had to include that you won't need. Here's what you can delete:

* Delete [netlify.toml](netlify.toml). It's used for deploying the demo website on Netlify.
* If you're using create-elm-app, delete [index.html](index.html). Be careful, make sure you delete [index.html at the root](index.html) and not [index.html in the public/ folder!](public/index.html)
* If you don't need service workers, delete [src/registerServiceWorker.js](src/registerServiceWorker.js)

### Alternatives to create-elm-app

Read this section if you don't want to use create-elm-app. Note that it's a fairly advanced topic.

There are many challenges regarding SPAs. Notably, you'll need a server that supports pushState navigation, 404 redirection, and correctly serving static assets even with nested URLs like `/nest1/nest2/42`. You may also run into issues trying to manage multiple environments and CI/CD.

For example, the popular [elm-live][elm live] correctly handles pushState and 404 redirection, but does not have native support for nested URLs. That is, if you use a relative path in HTML and refresh on a page like `/nest1/nest2/42`, your server will try to serve `/nest/nest1/42/src/index.js`, which doesn't exist. You'll need to dynamically indicate the "root" path of the server - the easiest way is probably to use an environment variable set to something like `PUBLIC_URL`, and use this variable in your `index.html` and anywhere else you need to refer to static files.

While setting an environment variable is trivial for local development, it can be a hassle when working with multiple environments, especially if you aren't sure what the PUBLIC_URL will be in your production environment. You might need to create scripts to identify the PUBLIC_URL or find libraries that do it for you - a lot of busy work we don't want to do!

create-elm-app and webpack handles all this for you, which is the main reason I chose to bootstrap this project with create-elm-app and would highly recommend sticking with it (or at the least, using webpack). It will save you a lot of work doing your own configuration and production minification/bundling. So unless you have your own pipeline already created, it will take a lot of work to use another tool!

The other option is to use hash-based routing. This will resolve issues with 404 redirection and nested URL paths, so you won't need environment variables and can use something like [elm-live][elm live] just fine.

 I have included a `index.html` in the root of the folder that you can use as a starting point for your custom configuration. Here's a generic outline of what you need to do:

* Compile the Elm code.

```none
elm make src/Main.elm --output=elm.js --debug
```

* Modify `src/index.js`. Comment out all the 'import' statements at the top of the file, except for the 'registerServiceWorker' line.

If using hash-based routing:

* Remove all instances of "%PUBLIC_URL%/" in index.html
* Modify Main.elm to use hash-based routing if you haven't already done so, as outlined in [#routing-with-hashes-(fragments)](#routing-with-hashes-(fragments)).
* Run your HTTP-server. Specify `./index.html` as the default file if required.

If using paths (`/`) as separator:

* Set the environment variable PUBLIC_URL to the path that your HTTP Server will be running - typically `PUBLIC_URL=""` is OK.
* Run your HTTP-server. Specify `./index.html` as the default file if required.

For production, minify and bundle your files using whatever tools you'd like. Be sure that your production server's environment variables are set appropriately and support SPAs.

## How it Works

This is an advanced section that discusses some of the design choices made in creating this boilerplate, why I made these choices, and possible alternatives.

### Inspiration

As I started working with Elm, one of the biggest things missing for me was a good boilerplate for SPAs. The Elm Guide only [briefly touches upon](https://guide.elm-lang.org/webapps/navigation.html) navigation and routing for SPAs, leaving most of the dirty work to be done by the reader. While there are some great examples of fully-fledged SPAs in Elm such as [Richard Feldman's RealWorld app](https://github.com/rtfeldman/elm-spa-example) and [the Elm Package site](https://github.com/elm/package.elm-lang.org), I found that if I tried to use one of these as a starting point, I had to clean the repo and remove all the extra files and features that I didn't need - it might take a couple hours before I could actually start writing my own code!

`elm-spa-boilerplate` looks to fill in this void. It includes all the necessary features like routing, navigation, localStorage and the file structure, but doesn't add any unnecessary clutter on top of that. Whether you're trying Elm for the first time or you're a seasoned Elm developer, this boilerplate is a great starting point for your next Elm project.

### Design Choices

(To be completed...)

## Contributing

Found an issue? Think there's an improvement to be made? Please feel free to open an issue on Github! I'd love to hear your thoughts and feedback.

I'd also love to see any applications that you create with this boilerplate!

## Acknowledgements

A special thank you to [lucamug] for introducing me to Elm as well as inspiring me with his own [Elm SPA boilerplate][luca spa] which was written for Elm 0.18.

This project borrows some concepts from [elm/package.elm-lang.org][elm package site] and [rtfeldman/elm-spa-example][rtfeldman spa]. It uses [create-elm-app][create elm app] for development and production.

The demo site is hosted on [Netlify]. If you haven't heard of Netlify, I highly recommend checking it out!

## License

This project is licensed under the MIT License, see [LICENSE](./LICENSE) for details.

<!-- Links -->
[elm install]: https://guide.elm-lang.org/install.html
[elm guide]: https://guide.elm-lang.org/
[elm ports]: https://guide.elm-lang.org/interop/ports.html
[elm live]: https://github.com/wking-io/elm-live
[elm package site]: https://github.com/elm/package.elm-lang.org
[rtfeldman spa]: https://github.com/rtfeldman/elm-spa-example
[create elm app]: https://github.com/halfzebra/create-elm-app
[create elm app docs]: https://github.com/halfzebra/create-elm-app/blob/master/template/README.md
[npm http server]: https://www.npmjs.com/package/http-server
[NodeJS]: https://nodejs.org/en/
[webpack]: https://webpack.js.org/
[Surge]: https://surge.sh/
[Netlify]: https://www.netlify.com/
[lucamug]: https://github.com/lucamug
[luca spa]: https://github.com/lucamug/elm-spa-boilerplate
