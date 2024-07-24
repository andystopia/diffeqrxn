{-# LANGUAGE LambdaCase #-}

module Cli where

import Options.Applicative

data ExportFormat = JuliaExport | JSONExport | TypstExport | AsciiExport deriving Show

newtype Subcommand = SubcommandExport ExportOpts deriving Show

exporterFormat :: ReadM ExportFormat
exporterFormat =
  str >>= \case
    "typst" -> return TypstExport
    "julia" -> return JuliaExport
    "json" -> return JSONExport
    "ascii" -> return AsciiExport
    _ -> readerError "Accepted export formats are typst, json, julia, and ascii"

data ExportOpts = ExportOpts
  { inputFile :: String,
    modelName :: String,
    exportFormat :: ExportFormat
  } deriving Show

cliParser :: ParserInfo Subcommand
cliParser =
  let parser =
        hsubparser (
          command
            "export"
            ( info
                ( (\inFile mName exFor -> SubcommandExport (ExportOpts inFile mName exFor))
                    <$> strArgument ( metavar "INPUT_FILE" <> help "Input Model File")
                    <*> strArgument ( metavar "MODEL_NAME" <> help "Model Name")
                    <*> option exporterFormat (long "format" <> short 'f' <> help "Export format. Must be typst, julia, json, or ascii")
                )
                (progDesc "Export models to a chosen format")
            ) 
            ) 
   in info
        (parser <**> helper)
        ( fullDesc
            <> header "Mass Action Kinetics exploratory model builder"
            <> progDesc "This program takes a model file (rxn) and can convert it to a number of defined formats. This makes it so that models have less copy and paste or just errors from typing them in, because the model writer can write down models piecewise, and only has to update one definition rather than multiple (in the case of writing down differential equations)"
        )

cli :: IO Subcommand
cli = execParser cliParser
