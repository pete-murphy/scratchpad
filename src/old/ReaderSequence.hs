module ReaderSequence where

data Value

internalChangeHandler :: Value -> IO ()
externalChangeHook :: Value -> IO ()

internalChangeHandler = undefined
externalChangeHook = undefined

handleChange :: Value -> [Value -> IO ()] -> IO ()
handleChange v = undefined