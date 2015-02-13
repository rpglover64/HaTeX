
import Text.LaTeX
import Text.LaTeX.Base.Parser
import Text.LaTeX.Base.Syntax (LaTeX(..), texmapM)

import Test.Tasty
import qualified Test.Tasty.QuickCheck as QC


instance Eq ParseError where
  _ == _ = undefined

eliminateRaw :: LaTeX -> Either ParseError LaTeX
eliminateRaw = texmapM isRaw (parseLaTeX . render)
  where isRaw (TeXRaw _) = True
        isRaw _          = False

main :: IO ()
main = defaultMain $ testGroup "HaTeX"
  [ testGroup "LaTeX"
    [ QC.testProperty "LaTeX mempty" $
         \l -> (mempty <> l) == (l <> mempty)
            && (mempty <> l) == (l :: LaTeX)
    , QC.testProperty "LaTeX mappend" $
         \l1 l2 l3 -> l1 <> (l2 <> l3) == (l1 <> l2) <> (l3 :: LaTeX)
    ]
  , testGroup "Parser"
    [ QC.testProperty "render . parse = id" $
         \l -> let t = render (l :: LaTeX)
               in  fmap render (parseLaTeX t) == Right t
    ]
  ]
