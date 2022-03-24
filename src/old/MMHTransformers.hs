import Control.Monad.Reader (Reader, ask, runReader)

main :: IO ()
main = do
  env <- loadEnv
  let str = runReader func1 env
  print str

data Environment
  = Environment
      { param1 :: String,
        param2 :: String,
        param3 :: String
      }

loadEnv :: IO Environment
loadEnv = undefined

func1 :: Reader Environment String
func1 = do
  res2 <- func2
  pure ("Result: " <> show res2)

func2 :: Reader Environment Int
func2 = do
  env <- ask
  let res3 = func3 env
  pure (2 + floor res3)

func3 :: Environment -> Double
func3 = undefined
