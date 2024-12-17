const path = require("path");
const webpack = require("webpack");
module.exports = {
  entry: {
    bundle: "./js/entry.js",
    "bundle.min": "./js/entry.js",
  },
  devtool: "source-map",
  resolve: {
    fallback: {
      buffer: require.resolve("buffer/"),
      assert: require.resolve("assert/"),
      stream: require.resolve("stream-browserify"),
    },
    extensions: ['.glsl', '.js', '.ts']
  },
  output: {
    path: path.resolve(__dirname, "dist"),
    library: { name: "bundle", type: "umd" },
  },
  plugins: [
    new webpack.ProvidePlugin({
      Buffer: ["buffer", "Buffer"],
    }),
    // fix "process is not defined" error; TODO may  be redundant with the "browser" tag now in package.json
    new webpack.ProvidePlugin({
      process: "process/browser",
    }),
  ],
  mode: "production",
  module: {
    rules: [
      {
        test: /\.glsl$/,
        use: {
          loader: 'webpack-glsl-minify',
          options: { output: 'source' } // this will output the minified version of the shader as a source file.  see:https://github.com/leosingleton/webpack-glsl-minify
        },
      }
    ]
  }
};
