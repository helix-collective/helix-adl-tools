const autoprefixer = require('autoprefixer');

function checkEnvironment(environment) {
  if (environment !== "dev" && environment !== "prod" && environment !== "storybook") {
    throw new Error("incorrect environment specified in webpack config: " + environment);
  }
}

module.exports = {
  cssLoaderUseStanza: function (environment) {
    checkEnvironment(environment);

    const stanza = [
      {
        loader: require.resolve('css-loader'),
        options: {
          importLoaders: 1,
          modules: true,
          localIdentName: '[name]__[local]____[hash:base64:5]',
          minimize: environment === "prod" ? true : undefined,
          sourceMap: environment === "prod" ? true : undefined,
        },
      },
      {
        loader: require.resolve('postcss-loader'),
        options: {
          // Necessary for external CSS imports to work
          // https://github.com/facebookincubator/create-react-app/issues/2677
          ident: 'postcss',
          plugins: () => [
            require('postcss-flexbugs-fixes'),
            autoprefixer({
              browsers: [
                '>1%',
                'last 4 versions',
                'Firefox ESR',
                'not ie < 9', // React doesn't support IE8 anyway
              ],
              flexbox: 'no-2009',
            }),
          ],
        },
      },
    ];

    if (environment !== "prod") {
      stanza.unshift(require.resolve('style-loader'));
    }

    return stanza;
  },
  fileLoaderConfig: function(environment) {
    checkEnvironment(environment);

    // "file" loader makes sure those assets get served by WebpackDevServer.
    // When you `import` an asset, you get its (virtual) filename.
    // In production, they would get copied to the `build` folder.
    return {
      // For storybook we explicitly specify the files to include to avoid
      // interference with storybook. Otherwise we catch all files.
      test: environment === "storybook" ? /\.(ttf|woff2?|eot|svg)$/ : undefined,
      // Exclude `js` files to keep "css" loader working as it injects
      // it's runtime that would otherwise processed through "file" loader.
      // Also exclude `html` and `json` extensions so they get processed
      // by webpacks internal loaders.
      exclude: [/\.js$/, /\.html$/, /\.json$/],
      loader: require.resolve('file-loader'),
      options: {
        name: 'static/media/[name].[hash:8].[ext]',
      },
    };
  },
}
