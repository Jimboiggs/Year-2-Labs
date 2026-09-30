mAnd :: Maybe Bool -> Maybe Bool -> Maybe Bool
mAnd (Just True) (Just True) = Just True
mAnd (Just True) (Just False) = Just False
mAnd (Just False) _ = Just False
mAnd _ (Just False) = Just False
mAnd _ _ = Nothing

mNot :: Maybe Bool -> Maybe Bool
mNot (Just True) = Just False
mNot (Just False) = Just True
mNot (Nothing) = Nothing

mOr :: Maybe Bool -> Maybe Bool -> Maybe Bool
mOr (Just True) (_) = Just True
mOr (_) (Just True) = Just True
mOr (Nothing) (Nothing) = Nothing

mImply :: Maybe Bool -> Maybe Bool -> Maybe Bool
mImply (Just True) (Just False) = Just False
mImply (_) (_) = Just True

mXor :: Maybe Bool -> Maybe Bool -> Maybe Bool
mXor (Just True) (Just False) = Just True
mXor (Just False) (Just True) = Just True
mXor (Nothing) (Nothing) = Nothing
mXor (_) (_) = Just False