data-serialization-dictionary
=============================

Data serialization for Map

Generalized serialization for dictionary, which can be used to produce serialization to/from Aeson, Map and other dictionary-based values.

Declare some data and derive from Generic:

<pre>
data Some = Some {
    someInt :: Int,
    someString :: String }
        deriving (Generic, Show)
</pre>

Then create instances to automatically convert fields to dictionary value type (String in this example)

<pre>
instance (Read a, Show a) =&gt; DictionaryValue String a where
    dictionaryValue = Convertible (Right . show) (Right . read)
</pre>

Derive from Serializable

<pre>
-- There are synonim Dictionarable k v for this type, but we can't use it here
instance Serializable (Codec (M.Map String String) (ToDictionary String String) (FromDictionary String String)) Some
</pre>

And use generated serializer with field names as in record

<pre>
some1 :: Dictionarable String String Some
some1 = ser
</pre>

Or declare your own serializer, changing any part of generated serializer

<pre>
some2 :: Dictionarable String String Some
some2 =
    dat_ (
        ctor_ (
            stor "Renamed first field" ser -- Rename field
            .*.
            gser)) -- Use generic serializer for this field
    .:.
    giso
</pre>

Using:

<pre>
encode some1 (Some 123 "hello") == Right (fromList [("someInt","123"),("someString","\"hello\"")])
encode some2 (Some 321 "world") == Right (fromList [("Renamed first field","321"),("someString","\"world\"")])
</pre>
