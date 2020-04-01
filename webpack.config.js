const path = require('path');
module.exports = {
  mode: 'development',
  devtool: 'inline-source-map',
  entry: './src/App.ts',
  output: {
    path: path.join(__dirname, 'dist'),
    filename: path.join('build', 'app.js'),
  },
  resolve: { extensions: ['.ts', '.js'] },
  devServer: {
    contentBase: path.join(__dirname, 'dist'),
    port: 7777,
  },
  module: {
    rules: [
      {
        test: /\.ts$/,
        loader: 'awesome-typescript-loader',
      },
      {
        test: /\.elm$/,
        exclude: [/elm-stuff/, /node_modules/],
        loaders: [
          {
            loader: 'elm-webpack-loader',
            options: {
              cwd: __dirname,
              debug: true,
            },
          },
        ],
      },
    ],
  },
};
