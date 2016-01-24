module.exports = {
  entry: './src/index.js',
  output: {
    path: './dist/',
    filename: 'bundle.js'
  },
  resolve: {
    extensions: ['', '.js']
  },
  module: {
    loaders: [{
      test: /\.elm$/,
      exclude: [/elm-stuff/, /node_modules/],
      loader: 'elm-webpack'
    }],
    noParse: [/\.elm$/]
  }
};
