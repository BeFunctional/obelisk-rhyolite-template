const path = require("path");
const webpack = require("webpack");
module.exports = {
  resolve: {
    alias: {
      'styled-components': path.resolve('./node_modules/styled-components')
    }
  },
  entry: {
    bundle: path.resolve("./js/entry.js"),
    "bundle.min": path.resolve("./js/entry.js"),
  },
  devtool: "source-map",
  module: {
    rules: [
      {
        test: /\.(js|ts|tsx)$/,
        loader: 'babel-loader',
        include: [path.join(__dirname, 'src'),
          /node_modules\/@kepler.gl/,
          /node_modules\/@loaders.gl/,
          /node_modules\/@probe.gl/,
          /node_modules\/@turf/,
          /node_modules\/@nebula.gl/,
          /node_modules\/polyclip-ts/,
          /node_modules\/splaytree-ts/
        ],
        options: {
          presets: [
            "@babel/preset-env",
            "@babel/preset-react",
            "@babel/preset-typescript"
          ],
          plugins: [
            "@babel/plugin-transform-class-properties"
          ]
        }
      },
      // fix for arrow-related errors
      {
        test: /\.mjs$/,
        include: /node_modules/,
        type: 'javascript/auto'
      }
    ]
  },
  node: {
    fs: 'empty'
  },
  output: {
    path: path.resolve(__dirname, "dist"),
    library: "bundle",
    libraryTarget: "umd"
  }
};
