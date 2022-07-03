{-# LANGUAGE UnliftedDatatypes #-}

module Data.HashMap.Mutable.Internal.Primes (getPrime, maxPrime) where

import Data.Primitive.Contiguous qualified as Array
import Data.Primitive.PrimArray (PrimArray, primArrayFromList)

getPrime :: Int -> Int
getPrime n
  | n >= maxPrime = n
  | otherwise = go 0
  where
    go !i = do
      let !p = Array.index primes i
      if p >= n
        then p
        else go $ i + 1
{-# INLINE getPrime #-}

maxPrime :: Int
maxPrime = Array.index primes $ Array.size primes - 1

{- ORMOLU_DISABLE -}
primes :: PrimArray Int
primes = primArrayFromList [
  7, 11, 13, 17, 23, 29, 37, 47,
  59, 73, 97, 127, 151, 197, 251, 313, 397,
  499, 631, 797, 1009, 1259, 1597, 2011, 2539,
  3203, 4027, 5087, 6421, 8089, 10193, 12853, 16193,
  20399, 25717, 32401, 40823, 51437, 64811, 81649,
  102877, 129607, 163307, 205759, 259229, 326617,
  411527, 518509, 653267, 823117, 1037059, 1306601,
  1646237, 2074129, 2613229, 3292489, 4148279, 5226491,
  6584983, 8296553, 10453007, 13169977, 16593127, 20906033,
  26339969, 33186281, 41812097, 52679969, 66372617,
  83624237, 105359939, 132745199, 167248483, 210719881,
  265490441, 334496971, 421439783, 530980861, 668993977,
  842879579, 1061961721, 1337987929, 1685759167, 2123923447,
  2675975881, 3371518343, 4247846927, 5351951779, 6743036717,
  8495693897, 10703903591, 13486073473, 16991387857,
  21407807219, 26972146961, 33982775741, 42815614441,
  53944293929, 67965551447, 85631228929, 107888587883,
  135931102921, 171262457903, 215777175787, 271862205833,
  342524915839, 431554351609, 543724411781, 685049831731,
  863108703229, 1087448823553, 1370099663459, 1726217406467,
  2174897647073, 2740199326961, 3452434812973, 4349795294267,
  5480398654009, 6904869625999, 8699590588571, 10960797308051,
  13809739252051, 17399181177241, 21921594616111, 27619478504183,
  34798362354533, 43843189232363, 55238957008387, 69596724709081,
  87686378464759, 110477914016779, 139193449418173,
  175372756929481, 220955828033581, 278386898836457,
  350745513859007, 441911656067171, 556773797672909,
  701491027718027, 883823312134381, 1113547595345903,
  1402982055436147, 1767646624268779, 2227095190691797,
  2805964110872297, 3535293248537579, 4454190381383713,
  5611928221744609, 7070586497075177, 8908380762767489,
  11223856443489329, 14141172994150357, 17816761525534927,
  22447712886978529, 28282345988300791, 35633523051069991,
  44895425773957261, 56564691976601587, 71267046102139967,
  89790851547914507, 113129383953203213, 142534092204280003,
  179581703095829107, 226258767906406483, 285068184408560057,
  359163406191658253, 452517535812813007, 570136368817120201,
  718326812383316683, 905035071625626043, 1140272737634240411,
  1436653624766633509, 1810070143251252131, 2280545475268481167,
  2873307249533267101, 3620140286502504283, 4561090950536962147,
  5746614499066534157, 7240280573005008577, 9122181901073924329
  ]
{- ORMOLU_ENABLE -}
