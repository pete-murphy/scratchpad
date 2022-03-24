data Spell = Spell String String String

splitOn :: String -> String -> [String]
splitOn = undefined

extractSpellId :: String -> Maybe String
extractSpellId = undefined

extractSpellName :: String -> Maybe String
extractSpellName = undefined

extractSpellDescription :: [String] -> Maybe String
extractSpellDescription = undefined

parseSpell' :: String -> String -> Maybe Spell
parseSpell' d s = do
  let parts = splitOn d s
  case extractSpellId $ parts !! 0 of
    Just spId ->
      case extractSpellName $ parts !! 0 of
        Just spName ->
          case extractSpellDescription parts of
            Just spDesc -> Just $ Spell spId spName spDesc
            _ -> Nothing
        _ -> Nothing
    _ -> Nothing

parseSpell :: String -> String -> Maybe Spell
parseSpell d s =
  Spell
    <$> (extractSpellId $ parts !! 0)
    <*> (extractSpellName $ parts !! 0)
    <*> (extractSpellDescription parts)
  where
    parts = splitOn d s
