var ELM_SOURCE_DIRECTORY = __dirname + '/src/';

module.exports = {
  entry: ELM_SOURCE_DIRECTORY + 'Main.elm',
  output: {
    path: __dirname,
    filename: 'bundle.js'
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
