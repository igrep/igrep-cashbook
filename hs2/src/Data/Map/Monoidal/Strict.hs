-- Avoid to depend on monoidal-containers,
-- which indirectly depends on template-haskell, which prevents me from building with asterius

{-
Copyright (c) 2015, Ben Gamari

All rights reserved.

Redistribution and use in source and binary forms, with or without
modification, are permitted provided that the following conditions are met:

    * Redistributions of source code must retain the above copyright
      notice, this list of conditions and the following disclaimer.

    * Redistributions in binary form must reproduce the above
      copyright notice, this list of conditions and the following
      disclaimer in the documentation and/or other materials provided
      with the distribution.

    * Neither the name of Ben Gamari nor the names of other
      contributors may be used to endorse or promote products derived
      from this software without specific prior written permission.

THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS
"AS IS" AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT
LIMITED TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR
A PARTICULAR PURPOSE ARE DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT
OWNER OR CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL,
SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT
LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS OF USE,
DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY
THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT
(INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE
OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.
-}

{-# LANGUAGE CPP #-}

module Data.Map.Monoidal.Strict
  ( MonoidalMap
  , assocs
  , singleton
  , elems
  ) where


import qualified Data.Map.Strict as M
#if !(MIN_VERSION_base(4,11,0))
import           Data.Semigroup
#endif


newtype MonoidalMap k a = MonoidalMap (M.Map k a) deriving (Eq, Show)

instance (Ord k, Semigroup a) => Semigroup (MonoidalMap k a) where
  MonoidalMap a <> MonoidalMap b = MonoidalMap $ M.unionWith (<>) a b
  {-# INLINE (<>) #-}

instance (Ord k, Semigroup a) => Monoid (MonoidalMap k a) where
  mempty = MonoidalMap mempty
  {-# INLINE mempty #-}
#if !(MIN_VERSION_base(4,11,0))
  mappend (MonoidalMap a) (MonoidalMap b) = MonoidalMap $ M.unionWith (<>) a b
  {-# INLINE mappend #-}
#endif


-- | /O(n)/. Return all elements of the map and their keys
assocs :: MonoidalMap k a -> [(k, a)]
assocs (MonoidalMap m) = M.assocs m
{-# INLINE assocs #-}


-- | /O(1)/. A map with a single element.
singleton :: k -> a -> MonoidalMap k a
singleton k = MonoidalMap . M.singleton k
{-# INLINE singleton #-}


-- | /O(n)/. Return all elements of the map in the ascending order of their
-- keys. Subject to list fusion.
elems :: MonoidalMap k a -> [a]
elems (MonoidalMap m) = M.elems m
{-# INLINE elems #-}
