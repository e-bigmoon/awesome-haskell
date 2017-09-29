18 Sep 2017 Michael Snoyman

The cryptonite library is the de facto standard in Haskell today for cryptography. It provides support for secure random number generic, shared key and public key encryption, message authentification codes (MACs), and—for our purposes today—cryptographic hashes.

For those unfamiliar: a hash function is a function that maps data from arbitrary size to a fixed size. A cryptographic hash function is a hash function with properties suitable for cryptography (see Wikipedia article for more details). A common example of cryptographic hash usage is providing a checksum on a file download to ensure it has not be tampered with. Common algorithms in use today include SHA256, Skein512, and (the slightly outdated) MD5.

The cryptonite library is built on top of the memory library, which provides type classes and convenience functions for dealing with reading and creating byte arrays. You may initially think "shouldn't it all be ByteStrings." We'll get to why the type classes are so helpful later.

Once familiar with these two libraries, they are straightforward to use. However, seeing how all the pieces fit together is difficult from just the API docs, especially understanding where an explicit type signature will be necessary. This post will give a quick overview of the pieces you'll want to be interacting with with simple, runnable examples. By the end, the goal is that you'll be able to trivially understand the API docs themselves.

The runnable examples below will all use the Stack script interpreter support. Make sure you have Stack installed and then, for each example:

Copy the contents into a file called Main.hs
Run stack Main.hs
Basic typeclasses
You're used to dealing with a a number of different string-like things, between strict and lazy bytestrings and text, plus Strings. However, if I asked you to tell me how to represent a strict sequence of bytes, you'd likely refer to Data.ByteString.ByteString. However, as you'll see through this tutorial, there are multiple data types we'll want to treat as a sequence of bytes.

The memory library defines two typeclasses to help out with this:

ByteArrayAccess gives you read-only access to the bytes within a data type.
ByteArray gives you read/write access, and is a child class of ByteArrayAccess.
To demonstrate, let's do a pointless conversion between ByteString and Bytes (which we'll explain in a second).

#!/usr/bin/env stack
-- stack --resolver lts-9.3 script
{-# LANGUAGE OverloadedStrings #-}
import qualified Data.ByteArray as BA
import Data.ByteString (ByteString)
import qualified Data.ByteString as B

main :: IO ()
main = do
  B.writeFile "test.txt" "This is a test"
  byteString <- B.readFile "test.txt"
  let bytes :: BA.Bytes
      bytes = BA.convert byteString
  print bytes
We're starting off with some file I/O using the bytestring library (because you should really do I/O with bytestring). Then the convert function can turn that into a Bytes value.

EXERCISE What do you think the type signature of convert is, given the description of the two typeclasses above? You can check if you're right.

Did you notice that explicit type signature I put on the bytes value? Well, that's your next lesson with memory and cryptonite: since so many functions work on type classes instead of concrete types, you'll often end up needing to give GHC some assistance on type inference.

I could show you an example of a data type which is a ByteArrayAccess but not a ByteArray, but it will ring hollow right now. When we get to actual hashing, the distinction in type classes will make a lot more sense. So let's just wait.

Why the different data types?
You may be legitimately wondering why there's a Bytes datatype in memory, when it seems identical to ByteString. In fact: it's not. Bytes has less memory overhead, which it gets by not tracking the offset and length of its slice. In exchange for that: a Bytes value doesn't allow for any slicing. In other words, the drop function on a Bytes would have to create a new copy of the buffer.

In other words: this is all performance stuff. And a library dealing with cryptography generally needs to be more concerned with performance.

Another interesting data type in memory is ScrubbedBytes. This type has three special properties (as called out in its Haddocks):

Being scrubbed after its goes out of scope.
A Show instance that doesn't actually show any content
A Eq instance that is constant time
In other words: it automatically prevents a number of common security holes when dealing with sensitive data.

OK, not much code to look at here, let's get to more fun stuff!

Base 16 encoding/decoding
Let's convert some user input into base 16 (aka hexadecimal):

#!/usr/bin/env stack
-- stack --resolver lts-9.3 script
import qualified Data.ByteArray          as BA
import           Data.ByteArray.Encoding (convertToBase, Base (Base16))
import           Data.ByteString         (ByteString)
import           Data.Text.Encoding      (encodeUtf8)
import qualified Data.Text.IO            as TIO
import           System.IO               (hFlush, stdout)

main :: IO ()
main = do
  putStr "Enter some text: "
  hFlush stdout
  text <- TIO.getLine
  let bs = encodeUtf8 text
  putStrLn $ "You entered: " ++ show bs
  let encoded = convertToBase Base16 bs :: ByteString
  putStrLn $ "Converted to base 16: " ++ show encoded
The convertToBase will convert the contents of any ByteArrayAccess into a ByteArray using the given base. Other options here besides Base16 include Base64 and others (just check out the docs).

As you can see, I had to put in an explicit ByteString type signature, since otherwise GHC wouldn't know which instance of ByteArrayAccess to use.

As you may guess, there is also a convertFromBase to do the opposite conversion. It returns an Either String byteArray value in case the input is not in the correct format.

EXERCISE Write a program to base 16 decode its input. (Solution follows.)

#!/usr/bin/env stack
-- stack --resolver lts-9.3 script
import qualified Data.ByteArray          as BA
import           Data.ByteArray.Encoding (convertFromBase, Base (Base16))
import           Data.ByteString         (ByteString)
import           Data.Text.Encoding      (encodeUtf8)
import qualified Data.Text.IO            as TIO
import           System.IO               (hFlush, stdout)

main :: IO ()
main = do
  putStr "Enter some hexadecimal text: "
  hFlush stdout
  text <- TIO.getLine
  let bs = encodeUtf8 text
  putStrLn $ "You entered: " ++ show bs
  case convertFromBase Base16 bs of
    Left e -> error $ "Invalid input: " ++ e
    Right decoded ->
      putStrLn $ "Converted from base 16: " ++ show (decoded :: ByteString)
EXERCISE Write a program to convert input from base 16 to base 64 encoding.

Hashing a strict bytestring
Alright, that's enough of the memory library. Time to do some real crypto stuff. We're going to get the SHA256 hash (aka digest) of some user input:

#!/usr/bin/env stack
-- stack --resolver lts-9.3 script
import           Crypto.Hash             (hash, SHA256 (..), Digest)
import           Data.ByteString         (ByteString)
import           Data.Text.Encoding      (encodeUtf8)
import qualified Data.Text.IO            as TIO
import           System.IO               (hFlush, stdout)

main :: IO ()
main = do
  putStr "Enter some text: "
  hFlush stdout
  text <- TIO.getLine
  let bs = encodeUtf8 text
  putStrLn $ "You entered: " ++ show bs
  let digest :: Digest SHA256
      digest = hash bs
  putStrLn $ "SHA256 hash: " ++ show digest
We've used the hash function to convert a ByteString (or any instance of ByteArrayAccess) into a Digest SHA256. If you're already wondering: yes, you could replace SHA256 with one of the other hash algorithms available.

As before: it's important to use a type signature of Digest SHA256 to let GHC know what kind of hash you want to perform. However, in this case, there's an alternative function you could choose instead:

#!/usr/bin/env stack
-- stack --resolver lts-9.3 script
import           Crypto.Hash             (hashWith, SHA256 (..))
import           Data.ByteString         (ByteString)
import           Data.Text.Encoding      (encodeUtf8)
import qualified Data.Text.IO            as TIO
import           System.IO               (hFlush, stdout)

main :: IO ()
main = do
  putStr "Enter some text: "
  hFlush stdout
  text <- TIO.getLine
  let bs = encodeUtf8 text
  putStrLn $ "You entered: " ++ show bs
  let digest = hashWith SHA256 bs
  putStrLn $ "SHA256 hash: " ++ show digest
The Show instance of Digest will display the digest in hexadecimal/base 16. That's pretty nice. But let's suppose we want to display it in base 64 instead. Get ready for this: Digest is an instance of ByteArrayAccess, so you can use convertToBase. (And it's not an instance of ByteArray, consider why such an instance would be problematic. If you're stumped: read the docs for this function for the answer.)

EXERCISE Display the digest as a base 64 encoded string (solution follows).

#!/usr/bin/env stack
-- stack --resolver lts-9.3 script
import           Crypto.Hash             (hashWith, SHA256 (..))
import           Data.ByteString         (ByteString)
import           Data.ByteArray.Encoding (convertToBase, Base (Base64))
import           Data.Text.Encoding      (encodeUtf8)
import qualified Data.Text.IO            as TIO
import           System.IO               (hFlush, stdout)

main :: IO ()
main = do
  putStr "Enter some text: "
  hFlush stdout
  text <- TIO.getLine
  let bs = encodeUtf8 text
  putStrLn $ "You entered: " ++ show bs
  let digest = convertToBase Base64 (hashWith SHA256 bs)
  putStrLn $ "SHA256 hash: " ++ show (digest :: ByteString)
Notice how we needed the type signature on digest to make it clear that it's a ByteString.

Check if any files match
Here's a neat little program. The user will provide a number of files as command line arguments. Then we'll print out lists of all the files with identical content (or, at least, matching SHA256s). (Try to notice something memory-inefficient in this implementation; we'll address it later.)

#!/usr/bin/env stack
-- stack --resolver lts-9.3 script
import           Crypto.Hash             (Digest, SHA256, hash)
import qualified Data.ByteString         as B
import           Data.Foldable           (forM_)
import           Data.Map.Strict         (Map)
import qualified Data.Map.Strict         as Map
import           System.Environment      (getArgs)

readFile' :: FilePath -> IO (Map (Digest SHA256) [FilePath])
readFile' fp = do
  bs <- B.readFile fp
  let digest = hash bs -- notice lack of type signature :)
  return $ Map.singleton digest [fp]

main :: IO ()
main = do
  args <- getArgs
  m <- Map.unionsWith (++) <$> mapM readFile' args
  forM_ (Map.toList m) $ \(digest, files) ->
    case files of
      [] -> error "can never happen"
      [_] -> return () -- only one file
      _ -> putStrLn $ show digest ++ ": " ++ unwords (map show files)
EXERCISE Write a program that will print out the SHA256 for every file name passed in on the command line.

QUESTION What's the inefficiency in the code above? You'll see in the next section.

More efficient file hashing
If we tried implementing our program from above without hashing, we'd either have to hold the entire file contents of each file in memory at once, or do some weird O(n^2) pairwise comparisons. Our hash-based implementation is better. But it's still a problem: it uses Data.ByteString.readFile, causing possibly unbounded memory usage. There's a more efficient way to hash entire files, using cryptonite-conduit:

#!/usr/bin/env stack
-- stack --resolver lts-9.3 script
import           Crypto.Hash         (Digest, SHA256, hash)
import           Crypto.Hash.Conduit (hashFile)
import           Data.Foldable       (forM_)
import           Data.Map.Strict     (Map)
import qualified Data.Map.Strict     as Map
import           System.Environment  (getArgs)

readFile' :: FilePath -> IO (Map (Digest SHA256) [FilePath])
readFile' fp = do
  digest <- hashFile fp
  return $ Map.singleton digest [fp]

main :: IO ()
main = do
  args <- getArgs
  m <- Map.unionsWith (++) <$> mapM readFile' args
  forM_ (Map.toList m) $ \(digest, files) ->
    case files of
      [] -> error "can never happen"
      [_] -> return () -- only one file
      _ -> putStrLn $ show digest ++ ": " ++ unwords (map show files)
Pretty simple change (in fact, I'd argue this code is just slightly easier to read), and we get far better memory performance (linear in the number of files being compared, constant in the size of those files).

Streaming hashing
Perhaps your ears (or eyes? you're probably reading this) perked up at the mention of conduit. To answer the question I'm going to pretend you're asking: yes, you can do streaming computation of a hash. Here's a program that will take a URL and file path, write the contents of the URL's response body to a file path, and print out the SHA256 digest. And the cool part: it will only look at each chunk of data once.

#!/usr/bin/env stack
-- stack --resolver lts-9.3 script
import Conduit
import Crypto.Hash         (Digest, SHA256, hash)
import Crypto.Hash.Conduit (sinkHash)
import Network.HTTP.Simple
import System.Environment  (getArgs)

main :: IO ()
main = do
  args <- getArgs
  (url, fp) <-
    case args of
      [x, y] -> return (x, y)
      _ -> error $ "Expected: URL FILEPATH"
  req <- parseRequest url
  digest <- runResourceT $ httpSink req $ \_res -> getZipSink $
    ZipSink (sinkFile fp) *>
    ZipSink sinkHash
  print (digest :: Digest SHA256)
Of course, if conduit can do it, you can do it too. Let's write a hashFile implementation ourselves without conduit to get some exposure to the raw hashing API:

#!/usr/bin/env stack
-- stack --resolver lts-9.3 script
import Crypto.Hash
import System.Environment  (getArgs)
import System.IO (withBinaryFile, IOMode (ReadMode))
import Data.Foldable (forM_)
import qualified Data.ByteString as B

hashFile :: HashAlgorithm ha => FilePath -> IO (Digest ha)
hashFile fp = withBinaryFile fp ReadMode $ \h ->
  let loop context = do
        chunk <- B.hGetSome h 4096
        if B.null chunk
          then return $ hashFinalize context
          else loop $! hashUpdate context chunk
   in loop hashInit

main :: IO ()
main = do
  args <- getArgs
  forM_ args $ \fp -> do
    digest <- hashFile fp
    putStrLn $ show (digest :: Digest SHA256) ++ "  " ++ fp
This uses the pure update API provided by Crypto.Hash. We can also use a mutating API in this case, which is slightly more efficient by bypassing some buffer copies:

#!/usr/bin/env stack
-- stack --resolver lts-9.3 script
import Crypto.Hash
import Crypto.Hash.IO
import System.Environment  (getArgs)
import System.IO (withBinaryFile, IOMode (ReadMode))
import Data.Foldable (forM_)
import qualified Data.ByteString as B

hashFile :: HashAlgorithm ha => FilePath -> IO (Digest ha)
hashFile fp = withBinaryFile fp ReadMode $ \h -> do
  context <- hashMutableInit
  let loop = do
        chunk <- B.hGetSome h 4096
        if B.null chunk
          then hashMutableFinalize context
          else do
            hashMutableUpdate context chunk
            loop
  loop

main :: IO ()
main = do
  args <- getArgs
  forM_ args $ \fp -> do
    digest <- hashFile fp
    putStrLn $ show (digest :: Digest SHA256) ++ "  " ++ fp
EXERCISE Use lazy I/O and the hashlazy function to implement hashFile. (NOTE: I am not condoning lazy I/O here.)
