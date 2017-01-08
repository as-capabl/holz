{-# LANGUAGE FlexibleContexts, FlexibleInstances, Rank2Types, LambdaCase #-}
{-# LANGUAGE ScopedTypeVariables, BangPatterns #-}
{-# LANGUAGE DataKinds, ConstraintKinds #-}
{-# LANGUAGE TypeFamilies #-}

---------------------------------------------------------------------------
-- |
-- Copyright   :
-- License     :  BSD-style (see the file LICENSE)
--
-- Maintainer  :  Fumiaki Kinoshita <fumiexcel@gmail.com>
-- Stability   :  experimental
-- Portability :  non-portable
--
-- The main API of holz
---------------------------------------------------------------------------

module Graphics.Holz.VSConfig
  ( VSConfig(..)
  , attrUniform
  , Vertex
  , vertexAttributes
  , bindAttribLocation
  , getUniformLocation
  , setInitUniform
  , VertexStore(..)
  ) where
import Data.Extensible
import qualified Data.Extensible as Xo
import Data.Proxy
import Foreign
import Foreign.C.String
import GHC.TypeLits
import Graphics.GL
import Linear
import Control.Monad.State
import Control.Monad.Identity

--
-- Primitive types to pass Open GL (OpenGL package has the same thing)
--
class Storable a => VertexAttrPrim a where
  primType :: proxy a -> GLenum
  primUniform1 :: GLint -> GLsizei -> Ptr a -> IO ()
  primUniform2 :: GLint -> GLsizei -> Ptr a -> IO ()
  primUniform3 :: GLint -> GLsizei -> Ptr a -> IO ()
  primUniform4 :: GLint -> GLsizei -> Ptr a -> IO ()

class Storable a => VertexAttr a where
  attrType :: proxy a -> GLenum
  attrSize :: proxy a -> GLint
  attrNormalized :: proxy a -> GLboolean
  attrUniform :: GLint -> GLsizei -> a -> IO ()

type AllVertexAttr = Forall (KeyValue KnownSymbol VertexAttr)

{-
instance VertexAttrPrim a => VertexAttr a where
  attrType = primType
  attrSize _ = 1
  attrNormalized _ = GL_FALSE
-}

instance VertexAttrPrim Int32 where
  primType _ = GL_INT
  primUniform1 = glUniform1iv
  primUniform2 = glUniform2iv
  primUniform3 = glUniform3iv
  primUniform4 = glUniform4iv

instance VertexAttr Int32 where
  attrType = primType
  attrSize _ = 1
  attrNormalized _ = GL_FALSE
  attrUniform loc count v = with v $ \p -> primUniform1 loc count (castPtr p :: Ptr Int32)

instance VertexAttrPrim Float where
  primType _ = GL_FLOAT
  primUniform1 = glUniform1fv
  primUniform2 = glUniform2fv
  primUniform3 = glUniform3fv
  primUniform4 = glUniform4fv

instance VertexAttr Float where
  attrType = primType
  attrSize _ = 1
  attrNormalized _ = GL_FALSE
  attrUniform loc count v = with v $ \p -> primUniform1 loc count (castPtr p :: Ptr Float)

instance VertexAttrPrim a => VertexAttr (V2 a) where
  attrType _ = primType (Proxy :: Proxy a)
  attrSize _ = 2
  attrNormalized _ = GL_FALSE
  attrUniform loc count v = with v $ \p -> primUniform2 loc count (castPtr p :: Ptr a)

instance VertexAttrPrim a => VertexAttr (V3 a) where
  attrType _ = primType (Proxy :: Proxy a)
  attrSize _ = 3
  attrNormalized _ = GL_FALSE
  attrUniform loc count v = with v $ \p -> primUniform3 loc count (castPtr p :: Ptr a)

instance VertexAttrPrim a => VertexAttr (V4 a) where
  attrType _ = primType (Proxy :: Proxy a)
  attrSize _ = 4
  attrNormalized _ = GL_FALSE
  attrUniform loc count v = with v $ \p -> primUniform4 loc count (castPtr p :: Ptr a)


--
-- Configuration
--
class (AllVertexAttr (Uniform conf), AllVertexAttr (Attribute conf))
      => VSConfig conf where
  type Uniform conf :: [Assoc Symbol *]
  type Attribute conf :: [Assoc Symbol *]
  initUniform :: conf -> UniformEnv conf
  vertexShaderSource :: conf -> String
  fragmentShaderSource :: conf -> String
  -- TODO: conf -> proxy conf

type Vertex conf = Record (Attribute conf)

type UniformEnv conf = Record (Uniform conf)

proxyKVVertexAttr :: Proxy (KeyValue KnownSymbol VertexAttr)
proxyKVVertexAttr = Proxy

vertexAttributes :: VSConfig conf => conf -> IO ()
vertexAttributes conf0 = do
  runStateT (Xo.hgenerateFor proxyKVVertexAttr (go conf0)) (0, nullPtr)
  return ()
  where
    go :: forall kv conf. (KeyValue KnownSymbol VertexAttr kv, VSConfig conf) =>
          conf -> Membership (Attribute conf) kv -> StateT (GLuint, Ptr ()) IO (Const' () kv)
    go conf ms = do
      (idx, ofs) <- get
      let pxv = Proxy :: Proxy (AssocValue kv)
          !idx' = idx + 1
          !ofs' = ofs `plusPtr` sizeOf (undefined :: AssocValue kv)
          stride = fromIntegral $ sizeOfRecord (undefined :: Vertex conf)
      liftIO $ glVertexAttribPointer idx
        (attrSize pxv) (attrType pxv) (attrNormalized pxv) stride ofs
      liftIO $ glEnableVertexAttribArray idx
      put (idx', ofs')
      return (Const' ())

bindAttribLocation :: VSConfig conf => conf -> GLuint -> IO ()
bindAttribLocation conf0 program = do
  runStateT (Xo.hgenerateFor proxyKVVertexAttr (go conf0)) 0
  return ()
  where
    go :: forall kv conf. (KeyValue KnownSymbol VertexAttr kv, VSConfig conf) =>
          conf -> Membership (Attribute conf) kv -> StateT GLuint IO (Const' () kv)
    go _ ms = do
      idx <- get
      let pxk = Proxy :: Proxy (AssocKey kv)
          !idx' = idx + 1
          varName = "in_" ++ symbolVal pxk
      liftIO $ withCString varName $ glBindAttribLocation program idx
      put idx'
      return (Const' ())

getUniformLocation :: VSConfig conf => conf -> GLuint -> IO [GLint]
getUniformLocation conf0 prog = do
  r <- execStateT (Xo.hgenerateFor proxyKVVertexAttr (go conf0)) []
  return $ reverse r
  where
    go :: forall kv conf. (KeyValue KnownSymbol VertexAttr kv, VSConfig conf) =>
          conf -> Membership (Uniform conf) kv -> StateT [GLint] IO (Const' () kv)
    go _ ms = do
      let pxk = Proxy :: Proxy (AssocKey kv)
          varName = "uni_" ++ symbolVal pxk
      r <- liftIO $ withCString varName $ glGetUniformLocation prog
      modify (r:)
      return (Const' ())

setInitUniform :: VSConfig conf => conf -> [GLint -> IO ()]
setInitUniform conf0 =
  reverse $ execState (Xo.hgenerateFor proxyKVVertexAttr (go conf0)) []
  where
    go :: forall kv conf. (KeyValue KnownSymbol VertexAttr kv, VSConfig conf) =>
          conf -> Membership (Uniform conf) kv -> State [GLint -> IO ()] (Const' () kv)
    go conf ms = do
      let x = runIdentity $ getField $ hlookup ms $ initUniform conf
          setInit loc = attrUniform loc 1 x
      modify (setInit :)
      return (Const' ())

--
-- To handle storable
--
newtype VertexStore conf = VertexStore { getVertexStore :: Vertex conf }

instance VSConfig conf => Storable (VertexStore conf) where
  sizeOf = sizeOfRecord . getVertexStore
  alignment _ = 0
  peek p = VertexStore <$> peekRecord (castPtr p)
  poke p x = pokeRecord (castPtr p) (getVertexStore x)

sizeOfRecord :: forall xs. AllVertexAttr xs => Record xs -> Int
sizeOfRecord rc = do
  execState (Xo.hgenerateFor proxyKVVertexAttr go) 0
  where
    go :: forall kv. (KeyValue KnownSymbol VertexAttr kv) =>
          Membership xs kv -> State Int (Const' () kv)
    go ms = do
      align <- get
      let !align' = align + sizeOf (runIdentity $ getField $ hlookup ms rc)
      put align'
      return $ Const' ()

peekRecord :: AllVertexAttr xs => Ptr (Record xs) -> IO (Record xs)
peekRecord ptr = do
  evalStateT (Xo.hgenerateFor proxyKVVertexAttr go) 0
  where
    go :: forall xs kv. (KeyValue KnownSymbol VertexAttr kv) =>
          Membership xs kv -> StateT Int IO (Field Identity kv)
    go ms = do
      align <- get
      x <- liftIO $ peek $ castPtr ptr `plusPtr` align
      let !align' = align + sizeOf x
      put align'
      return $ Field (Identity x)

pokeRecord :: forall xs. AllVertexAttr xs => Ptr (Record xs) -> Record xs -> IO ()
pokeRecord ptr rc = do
  evalStateT (Xo.hgenerateFor proxyKVVertexAttr go) 0
  return ()
  where
    go :: forall kv. (KeyValue KnownSymbol VertexAttr kv) =>
          Membership xs kv -> StateT Int IO (Const' () kv)
    go ms = do
      align <- get
      let x = runIdentity $ getField $ hlookup ms rc
          !align' = align + sizeOf x
      put align'
      liftIO $ poke (castPtr ptr `plusPtr` align) x
      return $ Const' ()


