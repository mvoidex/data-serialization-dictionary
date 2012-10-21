data-serialization-dictionary
=============================

Data serialization for Map

Generalized serialization for dictionary, which can be used to produce serialization to/from Aeson, Map and other dictionary-based values.

<pre>
data Result s a =
    ValidResult s a |
    InvalidResult String (Maybe a) |
    UnknownResult [String]
        deriving (Eq, Ord, Read, Show)

$(makeIso "result" ''Result)
-- ^ result :: Iso (Result s a) (Either (s, a) (Either (String, Maybe a) [String]))

test :: (
    DictionaryValue v s,
    DictionaryValue v a,
    DictionaryValue v String,
    DictionaryValue v [String])
    => Dictionarable Text v (Result s a)
test =
    (entry_ "result" .**. entry_ "state") .++.        -- ^ for ValidResult
    (entry_ "error" .**. try (entry_ "maybe")) .++.  -- ^ for InvalidResult
    entry_ "unknown"                                 -- ^ for UnknownResult
    .:.
    result                                            -- ^ Convert to Result s a

testJson :: (ToJSON s, FromJSON s, ToJSON a, FromJSON a) => Jsonable (Result s a)
testJson = dict test
</pre>
