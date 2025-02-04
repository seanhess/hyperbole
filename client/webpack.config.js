const path = require('path');
// var PACKAGE = require('./package.json');
// var version = PACKAGE.version;

module.exports = {
  entry: "./src/index.ts",
  mode: "development",
  target: "web",
  devtool: "source-map",
  mode: "production",
  module: {
    rules: [
      {
        test: /\.tsx?$/,
        use: 'ts-loader',
        exclude: /node_modules/,
      },
    ],
  },
  resolve: {
    mainFields: ['browser', 'module', 'main'],
    extensions: ['.tsx', '.ts', '.js'],
  },

  output: {
    // filename: `hyperbole-${version}.js`,
    filename: "hyperbole.js",
    path: path.resolve(__dirname, 'dist'),
  },

  // devServer: {
  //   contentBase: path.join(__dirname, 'dist'),
  //   compress: true,
  //   port: 9000,
  // },
}

