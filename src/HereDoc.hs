{-# OPTIONS_GHC -fno-warn-missing-fields #-}
module HereDoc (heredoc) where

import Language.Haskell.TH
import Language.Haskell.TH.Quote

heredoc :: QuasiQuoter
heredoc = QuasiQuoter {quoteExp = stringE}

