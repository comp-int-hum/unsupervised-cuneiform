module UnsupervisedCuneiform.Image (parseImage) where


import Data.ByteString.Lazy (ByteString, toStrict)
import Codec.Picture.Jpg (decodeJpegWithMetadata)
import Codec.Picture.Types (DynamicImage)
import Codec.Picture.Metadata (Metadatas)


parseImage :: ByteString -> Either String (DynamicImage, Metadatas)
parseImage = decodeJpegWithMetadata . toStrict
