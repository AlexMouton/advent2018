module Claims where

import Prelude

import Data.Array as A
import Data.Array.NonEmpty (NonEmptyArray, tail)
import Data.Either (fromRight)
import Data.Int.Parse (toRadix, parseInt)
import Data.List as L
import Data.Map as M
import Data.Maybe (Maybe(..))
import Data.Set as S
import Data.String (Pattern(..), split)
import Data.String.Regex (Regex, match, regex)
import Data.String.Regex.Flags (noFlags)
import Data.Tuple (Tuple(..))
import Data.Foldable (foldl)
import Data.Traversable (traverse, sequence)

import Partial.Unsafe (unsafePartial)

claims :: Maybe (Array Claim)
claims = traverse parseClaim $ split (Pattern "\n") source

type Claim = { id :: Int, x :: Int, y :: Int, w :: Int, h :: Int }
type Tile = { x :: Int, y :: Int }
type ClaimTile = { claim :: Int, tile :: Tile }
type TileClaims = M.Map Tile { ids :: Array Int }

toClaimTiles :: Claim -> Array ClaimTile
toClaimTiles c = toTiles c
  # map {claim: c.id, tile: _ }

toTiles :: Claim -> Array Tile
toTiles {x, y, w, h} = {x: _, y: _} <$> (A.range x (x+w-1)) <*> (A.range y (y+h-1))

hitTile :: TileClaims -> ClaimTile -> TileClaims
hitTile m ct =
  case M.member tile m of
    true -> M.update increment tile m
    false -> M.insert tile {ids: [ct.claim]} m
  where
    tile = ct.tile
    increment c@{ ids } = Just c{ ids= A.snoc ids ct.claim }

overlapClaims :: Array Claim -> TileClaims
overlapClaims = (=<<) toClaimTiles
  >>> foldl hitTile M.empty

overlapCount :: TileClaims -> Int
overlapCount =
  M.values
  >>> L.filter (_.ids >>> A.length >>> (<) 1)
  >>> L.length

overlappingClaims :: TileClaims -> S.Set Int
overlappingClaims =
  M.values
  >>> L.filter (_.ids >>> A.length >>> (<) 1)
  >>> A.fromFoldable
  >>> (=<<) (_.ids)
  >>> S.fromFoldable

candidateNonOverlappingClaims :: TileClaims -> S.Set Int
candidateNonOverlappingClaims =
  M.values
  >>> L.filter (_.ids >>> A.length >>> (==) 1)
  >>> A.fromFoldable
  >>> (=<<) (_.ids)
  >>> S.fromFoldable

nonOverlapping :: TileClaims -> Array Int
nonOverlapping ch =
  A.fromFoldable cnoc
  # A.filter (\a -> not $ S.member a oc )
  where
    cnoc = candidateNonOverlappingClaims ch
    oc = overlappingClaims ch

reClaim :: Regex
reClaim = unsafePartial $ fromRight $ regex "#([0-9]+) @ ([0-9]+),([0-9]+): ([0-9]+)x([0-9]+)" noFlags

parseDec :: String -> Maybe Int
parseDec s = parseInt s (toRadix 10)

parseClaim :: String -> Maybe Claim
parseClaim = numbers >=> buildClaim

buildClaim :: Array Int -> Maybe Claim
buildClaim ns =
  { id: _, x: _, y: _, w: _, h: _ }
  <$> lookup 0
  <*> lookup 1
  <*> lookup 2
  <*> lookup 3
  <*> lookup 4
  where
    lookup :: Int -> Maybe Int
    lookup i = A.index ns i

numbers :: String -> Maybe (Array Int)
numbers s =
  match reClaim s
  # map tail
  # map sequence
  # join
  # map (map parseDec)
  # map sequence
  # join

source :: String
source = """#1 @ 35,93: 11x13
#2 @ 518,811: 15x18
#3 @ 335,512: 11x10
#4 @ 635,155: 14x24
#5 @ 919,43: 10x16
#6 @ 391,695: 14x16
#7 @ 635,372: 17x17
#8 @ 289,489: 19x29
#9 @ 695,413: 10x25
#10 @ 730,730: 27x11
#11 @ 680,761: 29x22
#12 @ 44,685: 18x23
#13 @ 71,613: 28x16
#14 @ 292,456: 22x17
#15 @ 16,950: 14x10
#16 @ 778,140: 27x23
#17 @ 448,425: 22x22
#18 @ 681,770: 28x21
#19 @ 19,172: 23x21
#20 @ 542,333: 17x18
#21 @ 12,919: 14x20
#22 @ 466,752: 28x14
#23 @ 700,661: 18x21
#24 @ 582,738: 29x24
#25 @ 121,810: 15x10
#26 @ 38,378: 16x28
#27 @ 933,350: 20x22
#28 @ 467,605: 21x21
#29 @ 911,377: 16x15
#30 @ 339,469: 24x21
#31 @ 883,233: 29x23
#32 @ 133,22: 28x17
#33 @ 147,736: 15x11
#34 @ 630,705: 22x28
#35 @ 213,12: 28x11
#36 @ 615,523: 14x17
#37 @ 181,399: 10x19
#38 @ 643,493: 11x25
#39 @ 74,754: 14x21
#40 @ 932,169: 26x28
#41 @ 424,128: 11x23
#42 @ 457,341: 12x12
#43 @ 406,276: 18x13
#44 @ 449,292: 13x21
#45 @ 965,937: 28x22
#46 @ 868,454: 21x15
#47 @ 589,943: 26x19
#48 @ 911,937: 13x13
#49 @ 21,112: 16x13
#50 @ 970,201: 10x18
#51 @ 240,969: 18x25
#52 @ 611,475: 14x10
#53 @ 215,24: 14x13
#54 @ 470,491: 20x24
#55 @ 687,834: 25x10
#56 @ 672,772: 14x22
#57 @ 695,430: 23x12
#58 @ 490,483: 20x28
#59 @ 110,848: 18x12
#60 @ 698,533: 26x18
#61 @ 871,790: 20x23
#62 @ 272,752: 28x14
#63 @ 396,179: 17x15
#64 @ 213,1: 19x26
#65 @ 428,428: 21x22
#66 @ 250,254: 26x27
#67 @ 567,425: 12x10
#68 @ 505,414: 21x11
#69 @ 869,173: 14x12
#70 @ 707,936: 29x21
#71 @ 906,246: 29x12
#72 @ 148,3: 25x28
#73 @ 421,166: 25x26
#74 @ 597,916: 27x22
#75 @ 110,187: 25x19
#76 @ 199,770: 19x29
#77 @ 864,556: 21x20
#78 @ 677,6: 19x24
#79 @ 509,612: 17x16
#80 @ 610,78: 16x12
#81 @ 443,97: 15x19
#82 @ 417,198: 21x13
#83 @ 649,730: 27x27
#84 @ 560,934: 10x15
#85 @ 548,955: 16x13
#86 @ 375,738: 28x16
#87 @ 950,390: 20x20
#88 @ 539,260: 15x15
#89 @ 444,154: 16x19
#90 @ 382,3: 20x14
#91 @ 571,491: 24x29
#92 @ 915,437: 18x28
#93 @ 613,95: 23x13
#94 @ 666,356: 19x29
#95 @ 465,880: 25x10
#96 @ 106,734: 15x12
#97 @ 385,172: 20x24
#98 @ 659,510: 23x29
#99 @ 79,498: 12x10
#100 @ 236,259: 19x16
#101 @ 627,178: 15x23
#102 @ 948,329: 13x22
#103 @ 778,653: 15x17
#104 @ 923,812: 29x19
#105 @ 497,576: 13x24
#106 @ 671,683: 22x21
#107 @ 170,741: 24x12
#108 @ 565,963: 19x12
#109 @ 847,749: 16x26
#110 @ 309,951: 19x13
#111 @ 277,422: 17x28
#112 @ 366,159: 13x28
#113 @ 662,102: 14x24
#114 @ 729,374: 29x26
#115 @ 727,395: 29x26
#116 @ 864,693: 21x17
#117 @ 643,29: 21x11
#118 @ 225,214: 27x27
#119 @ 321,846: 21x16
#120 @ 166,795: 22x20
#121 @ 41,944: 21x23
#122 @ 259,252: 15x22
#123 @ 41,915: 14x19
#124 @ 103,38: 29x10
#125 @ 499,488: 12x28
#126 @ 442,319: 26x23
#127 @ 206,720: 11x25
#128 @ 165,665: 25x28
#129 @ 749,403: 9x12
#130 @ 544,63: 21x25
#131 @ 644,365: 13x24
#132 @ 732,51: 15x23
#133 @ 748,761: 18x22
#134 @ 256,834: 25x14
#135 @ 205,195: 24x10
#136 @ 425,941: 24x25
#137 @ 112,808: 18x24
#138 @ 416,355: 26x12
#139 @ 300,852: 25x17
#140 @ 967,31: 15x12
#141 @ 312,822: 10x19
#142 @ 84,920: 22x25
#143 @ 363,535: 24x13
#144 @ 246,968: 12x14
#145 @ 535,208: 20x23
#146 @ 682,352: 20x27
#147 @ 258,123: 18x27
#148 @ 14,283: 14x21
#149 @ 677,672: 29x18
#150 @ 854,133: 28x29
#151 @ 120,863: 25x14
#152 @ 617,629: 28x17
#153 @ 706,620: 10x18
#154 @ 495,941: 15x17
#155 @ 702,396: 12x24
#156 @ 603,322: 10x20
#157 @ 271,393: 28x15
#158 @ 848,523: 27x23
#159 @ 232,208: 10x25
#160 @ 331,514: 21x22
#161 @ 455,675: 14x11
#162 @ 955,843: 18x18
#163 @ 168,249: 5x16
#164 @ 489,609: 13x12
#165 @ 21,120: 29x21
#166 @ 682,483: 10x28
#167 @ 752,354: 20x20
#168 @ 281,469: 24x24
#169 @ 952,251: 26x10
#170 @ 673,91: 16x16
#171 @ 97,184: 14x29
#172 @ 289,164: 23x19
#173 @ 82,191: 11x29
#174 @ 915,595: 23x17
#175 @ 329,72: 16x24
#176 @ 105,384: 28x14
#177 @ 427,764: 29x19
#178 @ 476,147: 21x15
#179 @ 695,624: 29x28
#180 @ 753,352: 25x25
#181 @ 738,531: 17x22
#182 @ 876,844: 28x25
#183 @ 473,148: 21x18
#184 @ 552,963: 15x17
#185 @ 337,388: 28x20
#186 @ 579,961: 12x21
#187 @ 472,146: 14x24
#188 @ 100,401: 16x23
#189 @ 478,240: 15x20
#190 @ 664,254: 15x23
#191 @ 181,316: 23x20
#192 @ 910,377: 26x21
#193 @ 382,945: 19x19
#194 @ 378,818: 22x28
#195 @ 972,121: 20x24
#196 @ 138,344: 22x14
#197 @ 119,891: 16x17
#198 @ 715,605: 18x12
#199 @ 655,378: 25x12
#200 @ 38,617: 19x10
#201 @ 537,98: 11x19
#202 @ 848,233: 16x15
#203 @ 598,151: 17x10
#204 @ 359,442: 21x18
#205 @ 227,172: 18x24
#206 @ 805,778: 13x27
#207 @ 214,593: 14x10
#208 @ 943,328: 15x18
#209 @ 601,373: 13x11
#210 @ 734,509: 26x18
#211 @ 377,517: 18x21
#212 @ 423,284: 17x24
#213 @ 179,782: 25x21
#214 @ 537,528: 13x28
#215 @ 463,707: 27x11
#216 @ 496,614: 23x26
#217 @ 615,167: 27x16
#218 @ 562,784: 25x10
#219 @ 453,601: 17x23
#220 @ 684,610: 29x17
#221 @ 723,725: 19x17
#222 @ 395,634: 18x28
#223 @ 220,459: 22x10
#224 @ 371,816: 27x22
#225 @ 97,967: 12x21
#226 @ 859,548: 19x11
#227 @ 787,758: 29x18
#228 @ 171,792: 17x23
#229 @ 2,266: 23x21
#230 @ 974,22: 18x13
#231 @ 229,686: 18x18
#232 @ 940,713: 28x13
#233 @ 602,456: 11x13
#234 @ 700,953: 18x15
#235 @ 820,10: 14x19
#236 @ 623,507: 28x18
#237 @ 196,487: 11x25
#238 @ 487,349: 18x29
#239 @ 836,809: 18x27
#240 @ 461,289: 13x13
#241 @ 384,961: 16x26
#242 @ 678,670: 17x28
#243 @ 162,455: 17x25
#244 @ 7,931: 20x27
#245 @ 599,308: 29x29
#246 @ 649,497: 17x29
#247 @ 282,212: 21x18
#248 @ 20,841: 19x10
#249 @ 900,116: 13x3
#250 @ 486,189: 28x28
#251 @ 54,153: 24x17
#252 @ 461,108: 26x18
#253 @ 430,79: 22x27
#254 @ 447,384: 14x13
#255 @ 326,520: 29x26
#256 @ 900,371: 14x15
#257 @ 9,74: 25x10
#258 @ 844,895: 10x25
#259 @ 579,445: 12x25
#260 @ 781,711: 15x15
#261 @ 413,746: 14x16
#262 @ 727,898: 10x14
#263 @ 584,858: 21x28
#264 @ 586,17: 23x13
#265 @ 1,70: 24x27
#266 @ 289,778: 11x21
#267 @ 603,364: 21x29
#268 @ 482,161: 20x11
#269 @ 941,508: 16x27
#270 @ 364,742: 21x14
#271 @ 34,229: 10x29
#272 @ 665,277: 10x19
#273 @ 301,702: 27x23
#274 @ 114,169: 10x20
#275 @ 565,278: 27x15
#276 @ 297,931: 15x26
#277 @ 173,640: 11x25
#278 @ 844,238: 27x19
#279 @ 287,477: 10x17
#280 @ 204,851: 10x12
#281 @ 745,401: 21x22
#282 @ 831,770: 17x13
#283 @ 718,963: 21x22
#284 @ 733,216: 13x11
#285 @ 439,150: 28x12
#286 @ 377,633: 13x20
#287 @ 166,244: 10x28
#288 @ 28,943: 20x25
#289 @ 108,269: 20x18
#290 @ 725,619: 17x25
#291 @ 284,214: 15x13
#292 @ 715,429: 10x25
#293 @ 306,46: 17x22
#294 @ 131,815: 19x12
#295 @ 684,355: 18x14
#296 @ 372,163: 10x17
#297 @ 126,202: 17x27
#298 @ 246,131: 20x28
#299 @ 345,422: 11x24
#300 @ 190,742: 23x25
#301 @ 469,360: 22x18
#302 @ 332,407: 25x15
#303 @ 125,45: 15x13
#304 @ 596,852: 20x29
#305 @ 980,525: 14x19
#306 @ 166,830: 18x25
#307 @ 579,439: 11x12
#308 @ 609,93: 11x26
#309 @ 671,170: 27x23
#310 @ 952,206: 15x15
#311 @ 668,519: 25x28
#312 @ 227,444: 27x27
#313 @ 876,548: 24x20
#314 @ 484,105: 25x25
#315 @ 859,733: 25x25
#316 @ 114,198: 21x21
#317 @ 535,902: 29x11
#318 @ 553,473: 25x17
#319 @ 52,143: 18x23
#320 @ 267,859: 26x24
#321 @ 555,315: 16x27
#322 @ 811,773: 26x16
#323 @ 57,159: 20x12
#324 @ 32,132: 19x25
#325 @ 906,254: 23x23
#326 @ 163,573: 19x19
#327 @ 943,848: 19x11
#328 @ 316,376: 13x25
#329 @ 870,275: 28x26
#330 @ 736,213: 18x17
#331 @ 515,105: 25x19
#332 @ 58,505: 27x16
#333 @ 657,552: 17x24
#334 @ 741,885: 19x20
#335 @ 178,663: 27x24
#336 @ 94,172: 17x25
#337 @ 256,653: 27x26
#338 @ 63,814: 23x10
#339 @ 678,836: 18x29
#340 @ 29,728: 4x3
#341 @ 416,327: 12x20
#342 @ 699,230: 13x10
#343 @ 920,4: 21x24
#344 @ 352,434: 16x4
#345 @ 861,757: 25x12
#346 @ 532,715: 23x29
#347 @ 520,414: 26x22
#348 @ 211,217: 14x22
#349 @ 593,37: 19x10
#350 @ 309,471: 18x24
#351 @ 865,127: 12x19
#352 @ 368,570: 19x19
#353 @ 126,601: 29x28
#354 @ 605,665: 12x26
#355 @ 58,600: 10x29
#356 @ 906,365: 20x24
#357 @ 743,291: 13x29
#358 @ 15,808: 18x10
#359 @ 345,379: 25x19
#360 @ 482,740: 21x27
#361 @ 812,46: 13x19
#362 @ 168,121: 14x27
#363 @ 782,288: 28x18
#364 @ 421,487: 22x16
#365 @ 266,985: 24x11
#366 @ 699,373: 26x10
#367 @ 814,919: 29x25
#368 @ 294,174: 26x18
#369 @ 976,70: 16x24
#370 @ 101,159: 16x19
#371 @ 236,822: 23x22
#372 @ 591,232: 17x23
#373 @ 946,403: 15x16
#374 @ 611,252: 26x24
#375 @ 393,150: 21x15
#376 @ 82,138: 11x18
#377 @ 551,765: 16x25
#378 @ 159,172: 28x27
#379 @ 597,356: 17x14
#380 @ 643,858: 22x18
#381 @ 441,663: 21x17
#382 @ 683,813: 21x24
#383 @ 15,335: 19x14
#384 @ 478,786: 26x13
#385 @ 328,270: 26x26
#386 @ 824,375: 15x12
#387 @ 279,774: 14x26
#388 @ 952,907: 18x14
#389 @ 607,921: 13x16
#390 @ 588,157: 20x14
#391 @ 923,379: 20x29
#392 @ 262,445: 20x28
#393 @ 476,233: 14x26
#394 @ 317,574: 11x29
#395 @ 915,495: 20x20
#396 @ 667,858: 17x26
#397 @ 147,922: 28x19
#398 @ 271,787: 29x21
#399 @ 636,571: 23x17
#400 @ 945,628: 14x14
#401 @ 747,895: 12x22
#402 @ 598,194: 24x17
#403 @ 309,391: 10x13
#404 @ 760,192: 28x17
#405 @ 307,702: 21x15
#406 @ 954,405: 20x12
#407 @ 242,191: 28x20
#408 @ 384,537: 22x17
#409 @ 600,946: 20x10
#410 @ 699,310: 23x27
#411 @ 822,14: 9x8
#412 @ 797,166: 28x24
#413 @ 102,605: 18x10
#414 @ 677,233: 26x20
#415 @ 68,36: 29x25
#416 @ 606,606: 15x26
#417 @ 9,59: 22x26
#418 @ 421,945: 12x22
#419 @ 531,798: 21x23
#420 @ 481,950: 18x21
#421 @ 779,299: 16x29
#422 @ 230,90: 15x18
#423 @ 494,110: 17x23
#424 @ 452,349: 12x12
#425 @ 599,615: 15x26
#426 @ 924,612: 20x28
#427 @ 519,647: 29x22
#428 @ 980,524: 12x14
#429 @ 246,825: 26x27
#430 @ 706,570: 20x29
#431 @ 105,58: 21x10
#432 @ 211,667: 25x15
#433 @ 819,495: 17x29
#434 @ 757,658: 11x24
#435 @ 955,937: 21x29
#436 @ 656,255: 23x17
#437 @ 645,355: 28x24
#438 @ 280,950: 15x15
#439 @ 834,119: 25x22
#440 @ 337,330: 23x25
#441 @ 676,731: 13x28
#442 @ 562,9: 26x23
#443 @ 416,340: 12x19
#444 @ 7,869: 10x13
#445 @ 305,77: 26x11
#446 @ 594,790: 25x11
#447 @ 813,35: 25x21
#448 @ 306,499: 22x17
#449 @ 461,290: 12x27
#450 @ 122,806: 23x23
#451 @ 938,617: 18x22
#452 @ 156,709: 20x19
#453 @ 638,755: 12x13
#454 @ 412,296: 15x27
#455 @ 942,890: 16x28
#456 @ 64,613: 22x12
#457 @ 929,3: 14x17
#458 @ 684,260: 22x29
#459 @ 263,115: 21x18
#460 @ 278,189: 27x15
#461 @ 156,517: 26x12
#462 @ 843,889: 13x25
#463 @ 920,27: 16x27
#464 @ 911,312: 19x16
#465 @ 157,514: 14x23
#466 @ 449,891: 13x16
#467 @ 692,921: 18x24
#468 @ 228,769: 27x22
#469 @ 131,816: 21x28
#470 @ 446,453: 18x29
#471 @ 750,708: 15x29
#472 @ 886,781: 25x25
#473 @ 343,530: 17x20
#474 @ 361,681: 14x24
#475 @ 737,928: 27x15
#476 @ 574,467: 14x20
#477 @ 896,0: 20x15
#478 @ 849,138: 10x16
#479 @ 302,12: 17x29
#480 @ 633,407: 24x21
#481 @ 282,514: 26x10
#482 @ 290,279: 22x26
#483 @ 442,84: 25x10
#484 @ 399,964: 28x10
#485 @ 567,453: 17x10
#486 @ 522,829: 28x16
#487 @ 121,122: 26x12
#488 @ 877,935: 19x15
#489 @ 312,59: 22x10
#490 @ 777,361: 21x28
#491 @ 111,884: 21x15
#492 @ 25,928: 15x28
#493 @ 371,649: 11x19
#494 @ 800,887: 28x15
#495 @ 935,338: 27x12
#496 @ 857,221: 15x27
#497 @ 582,799: 27x12
#498 @ 382,495: 23x22
#499 @ 320,274: 19x23
#500 @ 627,533: 18x11
#501 @ 325,537: 29x16
#502 @ 699,341: 22x15
#503 @ 76,835: 28x24
#504 @ 598,175: 20x13
#505 @ 948,618: 28x26
#506 @ 886,515: 11x18
#507 @ 223,494: 26x19
#508 @ 374,883: 13x22
#509 @ 936,562: 27x28
#510 @ 685,684: 13x16
#511 @ 937,178: 12x7
#512 @ 21,675: 18x10
#513 @ 411,58: 25x20
#514 @ 585,845: 23x26
#515 @ 124,162: 24x29
#516 @ 730,291: 18x14
#517 @ 678,521: 29x29
#518 @ 655,374: 16x18
#519 @ 592,325: 15x12
#520 @ 736,305: 29x13
#521 @ 257,731: 18x28
#522 @ 468,853: 14x14
#523 @ 170,917: 15x17
#524 @ 421,751: 22x20
#525 @ 503,958: 15x22
#526 @ 9,517: 24x16
#527 @ 497,478: 23x10
#528 @ 471,297: 29x10
#529 @ 409,706: 11x13
#530 @ 422,67: 14x21
#531 @ 123,155: 20x17
#532 @ 947,527: 12x26
#533 @ 42,588: 17x17
#534 @ 21,510: 10x9
#535 @ 709,616: 13x19
#536 @ 420,132: 22x22
#537 @ 157,722: 10x24
#538 @ 626,639: 18x18
#539 @ 672,182: 16x10
#540 @ 910,365: 26x28
#541 @ 64,411: 16x20
#542 @ 744,720: 27x12
#543 @ 856,59: 20x13
#544 @ 866,210: 28x13
#545 @ 921,118: 14x17
#546 @ 86,440: 23x24
#547 @ 27,723: 16x13
#548 @ 932,409: 26x29
#549 @ 944,256: 21x27
#550 @ 544,269: 20x16
#551 @ 75,618: 14x26
#552 @ 130,386: 29x16
#553 @ 847,434: 19x25
#554 @ 667,523: 13x12
#555 @ 656,373: 24x20
#556 @ 596,740: 5x18
#557 @ 813,381: 15x27
#558 @ 936,45: 18x29
#559 @ 414,275: 26x28
#560 @ 66,461: 11x26
#561 @ 333,76: 20x29
#562 @ 375,261: 21x10
#563 @ 151,206: 21x11
#564 @ 112,281: 29x21
#565 @ 734,470: 25x26
#566 @ 361,304: 27x23
#567 @ 348,904: 25x22
#568 @ 109,600: 12x11
#569 @ 869,127: 22x12
#570 @ 716,771: 26x21
#571 @ 222,585: 13x12
#572 @ 670,25: 12x14
#573 @ 173,133: 11x28
#574 @ 186,271: 27x14
#575 @ 862,937: 27x25
#576 @ 460,854: 20x29
#577 @ 213,188: 15x27
#578 @ 468,715: 19x11
#579 @ 768,258: 12x28
#580 @ 512,758: 29x29
#581 @ 712,749: 15x27
#582 @ 734,693: 11x17
#583 @ 480,973: 3x6
#584 @ 639,932: 20x16
#585 @ 924,305: 19x26
#586 @ 925,419: 26x28
#587 @ 609,299: 25x16
#588 @ 7,916: 11x24
#589 @ 417,717: 13x24
#590 @ 668,270: 26x28
#591 @ 92,206: 10x15
#592 @ 469,971: 27x16
#593 @ 363,701: 29x27
#594 @ 871,749: 14x25
#595 @ 603,418: 28x19
#596 @ 980,117: 18x29
#597 @ 493,485: 20x12
#598 @ 291,169: 17x10
#599 @ 660,38: 26x27
#600 @ 884,297: 19x19
#601 @ 899,721: 10x16
#602 @ 345,104: 19x20
#603 @ 439,390: 26x11
#604 @ 364,59: 26x18
#605 @ 668,495: 21x29
#606 @ 252,398: 14x24
#607 @ 271,515: 26x24
#608 @ 66,258: 26x21
#609 @ 64,204: 23x26
#610 @ 563,68: 15x11
#611 @ 205,956: 27x19
#612 @ 599,35: 20x18
#613 @ 694,613: 25x20
#614 @ 484,279: 22x23
#615 @ 298,589: 20x22
#616 @ 147,568: 23x25
#617 @ 904,171: 22x20
#618 @ 66,279: 26x22
#619 @ 916,593: 14x27
#620 @ 326,202: 13x29
#621 @ 465,438: 25x15
#622 @ 19,508: 16x20
#623 @ 600,468: 16x10
#624 @ 169,762: 23x24
#625 @ 667,188: 18x22
#626 @ 332,706: 26x24
#627 @ 446,50: 15x27
#628 @ 628,937: 22x15
#629 @ 649,864: 14x16
#630 @ 744,749: 23x17
#631 @ 264,182: 11x28
#632 @ 373,228: 11x27
#633 @ 223,589: 21x13
#634 @ 378,259: 16x16
#635 @ 25,805: 21x21
#636 @ 573,735: 28x18
#637 @ 866,573: 22x26
#638 @ 512,834: 25x11
#639 @ 240,753: 19x14
#640 @ 502,200: 22x14
#641 @ 292,613: 10x17
#642 @ 709,499: 26x23
#643 @ 829,412: 17x13
#644 @ 893,260: 27x17
#645 @ 265,166: 28x17
#646 @ 791,188: 29x23
#647 @ 256,497: 28x27
#648 @ 555,847: 20x25
#649 @ 35,242: 12x18
#650 @ 537,853: 19x24
#651 @ 33,122: 11x16
#652 @ 375,520: 13x19
#653 @ 458,490: 18x11
#654 @ 609,412: 16x19
#655 @ 58,364: 25x20
#656 @ 84,826: 23x15
#657 @ 490,581: 20x14
#658 @ 508,284: 15x19
#659 @ 819,777: 13x26
#660 @ 36,69: 4x3
#661 @ 126,338: 21x29
#662 @ 270,615: 25x12
#663 @ 62,217: 28x20
#664 @ 295,183: 18x10
#665 @ 192,762: 29x11
#666 @ 564,31: 15x21
#667 @ 80,744: 13x19
#668 @ 793,162: 24x28
#669 @ 448,648: 16x14
#670 @ 801,905: 14x16
#671 @ 585,6: 25x25
#672 @ 721,451: 25x19
#673 @ 900,752: 15x22
#674 @ 83,739: 25x12
#675 @ 738,162: 24x16
#676 @ 925,232: 17x24
#677 @ 753,516: 18x18
#678 @ 253,329: 12x27
#679 @ 452,44: 24x26
#680 @ 675,358: 27x15
#681 @ 558,932: 15x29
#682 @ 358,147: 28x18
#683 @ 895,109: 25x14
#684 @ 761,160: 3x4
#685 @ 3,942: 13x29
#686 @ 482,197: 22x27
#687 @ 665,194: 25x15
#688 @ 439,173: 27x11
#689 @ 237,761: 17x27
#690 @ 883,776: 26x18
#691 @ 814,135: 12x12
#692 @ 36,947: 18x14
#693 @ 341,737: 23x10
#694 @ 588,186: 19x17
#695 @ 710,437: 22x29
#696 @ 870,928: 27x28
#697 @ 362,374: 29x23
#698 @ 165,6: 14x24
#699 @ 440,426: 22x13
#700 @ 622,435: 16x22
#701 @ 350,431: 21x26
#702 @ 98,385: 11x29
#703 @ 250,677: 18x28
#704 @ 633,939: 17x27
#705 @ 950,410: 23x24
#706 @ 766,503: 23x18
#707 @ 973,821: 13x19
#708 @ 8,132: 28x25
#709 @ 444,639: 25x13
#710 @ 588,68: 23x11
#711 @ 186,42: 25x11
#712 @ 898,755: 25x11
#713 @ 86,828: 16x5
#714 @ 282,398: 24x22
#715 @ 892,359: 28x20
#716 @ 707,209: 27x20
#717 @ 61,811: 18x22
#718 @ 335,512: 19x27
#719 @ 412,743: 23x19
#720 @ 260,261: 12x24
#721 @ 202,758: 18x29
#722 @ 297,299: 11x16
#723 @ 49,807: 19x12
#724 @ 922,272: 3x12
#725 @ 965,487: 19x18
#726 @ 120,798: 11x13
#727 @ 778,747: 22x16
#728 @ 39,120: 13x24
#729 @ 207,482: 24x13
#730 @ 123,116: 22x12
#731 @ 156,7: 15x26
#732 @ 90,934: 6x5
#733 @ 573,727: 28x29
#734 @ 166,179: 16x14
#735 @ 932,484: 29x25
#736 @ 608,950: 15x27
#737 @ 734,909: 25x26
#738 @ 544,837: 13x25
#739 @ 529,348: 18x20
#740 @ 653,940: 17x11
#741 @ 637,33: 10x27
#742 @ 712,548: 19x17
#743 @ 477,469: 22x23
#744 @ 713,331: 11x13
#745 @ 19,4: 10x18
#746 @ 599,251: 29x10
#747 @ 134,306: 28x18
#748 @ 190,522: 24x12
#749 @ 212,253: 18x22
#750 @ 746,477: 15x10
#751 @ 17,762: 13x13
#752 @ 625,505: 9x8
#753 @ 671,695: 25x10
#754 @ 937,712: 13x24
#755 @ 933,401: 27x14
#756 @ 806,135: 22x11
#757 @ 75,41: 27x26
#758 @ 2,937: 20x13
#759 @ 333,691: 19x18
#760 @ 932,797: 29x23
#761 @ 415,934: 17x28
#762 @ 355,584: 24x11
#763 @ 680,297: 26x26
#764 @ 880,512: 13x11
#765 @ 180,385: 19x20
#766 @ 21,829: 26x28
#767 @ 327,650: 28x18
#768 @ 286,730: 15x15
#769 @ 962,828: 24x11
#770 @ 523,754: 20x25
#771 @ 204,159: 27x27
#772 @ 428,205: 21x13
#773 @ 818,894: 23x25
#774 @ 644,611: 18x20
#775 @ 160,640: 25x21
#776 @ 244,958: 16x12
#777 @ 65,160: 10x18
#778 @ 351,900: 25x14
#779 @ 943,635: 13x15
#780 @ 90,836: 27x25
#781 @ 371,178: 18x15
#782 @ 661,297: 22x14
#783 @ 374,679: 13x26
#784 @ 380,561: 12x29
#785 @ 579,865: 16x13
#786 @ 318,509: 19x13
#787 @ 765,493: 25x22
#788 @ 384,894: 17x28
#789 @ 253,964: 16x26
#790 @ 502,288: 19x11
#791 @ 636,823: 12x18
#792 @ 813,347: 10x14
#793 @ 617,299: 19x11
#794 @ 314,837: 29x19
#795 @ 555,767: 16x26
#796 @ 813,483: 25x23
#797 @ 930,561: 20x12
#798 @ 677,270: 23x16
#799 @ 134,356: 17x27
#800 @ 515,877: 26x27
#801 @ 412,825: 15x14
#802 @ 247,278: 25x25
#803 @ 176,769: 23x22
#804 @ 129,174: 26x29
#805 @ 87,243: 14x26
#806 @ 746,259: 28x25
#807 @ 616,422: 14x27
#808 @ 58,583: 15x20
#809 @ 584,305: 20x24
#810 @ 156,483: 18x22
#811 @ 578,429: 29x16
#812 @ 489,235: 12x10
#813 @ 958,742: 23x29
#814 @ 212,689: 19x28
#815 @ 113,557: 20x23
#816 @ 895,161: 20x21
#817 @ 564,41: 12x28
#818 @ 418,184: 12x26
#819 @ 436,441: 16x23
#820 @ 961,873: 10x13
#821 @ 709,511: 22x19
#822 @ 232,443: 19x18
#823 @ 878,773: 13x23
#824 @ 186,47: 11x13
#825 @ 386,466: 24x27
#826 @ 1,482: 17x20
#827 @ 442,86: 11x20
#828 @ 10,676: 20x20
#829 @ 964,439: 21x13
#830 @ 418,971: 26x19
#831 @ 128,619: 29x14
#832 @ 959,763: 21x20
#833 @ 148,163: 29x22
#834 @ 197,510: 22x10
#835 @ 908,179: 19x22
#836 @ 637,973: 10x15
#837 @ 378,0: 14x20
#838 @ 724,40: 27x21
#839 @ 903,617: 29x11
#840 @ 593,861: 10x10
#841 @ 239,72: 26x24
#842 @ 360,682: 21x18
#843 @ 411,752: 21x28
#844 @ 92,235: 20x24
#845 @ 900,231: 11x24
#846 @ 718,290: 22x11
#847 @ 217,968: 21x11
#848 @ 461,863: 21x15
#849 @ 825,873: 11x21
#850 @ 885,52: 16x24
#851 @ 686,652: 14x21
#852 @ 2,856: 11x23
#853 @ 495,798: 25x24
#854 @ 15,161: 24x28
#855 @ 645,423: 22x19
#856 @ 198,499: 23x25
#857 @ 697,376: 13x13
#858 @ 489,885: 13x21
#859 @ 504,623: 21x28
#860 @ 347,586: 10x11
#861 @ 12,16: 10x19
#862 @ 847,64: 22x22
#863 @ 823,880: 27x20
#864 @ 892,59: 24x13
#865 @ 210,10: 15x12
#866 @ 397,773: 28x24
#867 @ 19,478: 25x26
#868 @ 289,243: 18x24
#869 @ 239,951: 13x24
#870 @ 621,278: 19x23
#871 @ 701,546: 24x26
#872 @ 577,952: 21x10
#873 @ 812,717: 13x13
#874 @ 109,329: 11x26
#875 @ 187,546: 12x12
#876 @ 417,117: 13x24
#877 @ 362,221: 22x20
#878 @ 594,570: 28x19
#879 @ 418,782: 19x13
#880 @ 627,429: 29x10
#881 @ 683,523: 12x22
#882 @ 965,943: 19x15
#883 @ 50,407: 29x20
#884 @ 513,662: 29x25
#885 @ 9,930: 17x15
#886 @ 164,795: 17x24
#887 @ 223,349: 10x17
#888 @ 792,690: 25x23
#889 @ 15,305: 13x18
#890 @ 1,930: 21x29
#891 @ 14,921: 11x14
#892 @ 252,354: 23x11
#893 @ 527,211: 18x16
#894 @ 230,679: 15x20
#895 @ 265,845: 16x29
#896 @ 547,92: 17x13
#897 @ 102,221: 17x19
#898 @ 480,239: 27x10
#899 @ 9,54: 12x21
#900 @ 721,630: 19x12
#901 @ 120,901: 12x14
#902 @ 36,369: 26x15
#903 @ 755,156: 18x29
#904 @ 864,806: 15x22
#905 @ 395,892: 15x27
#906 @ 351,440: 25x15
#907 @ 403,357: 28x15
#908 @ 184,728: 17x27
#909 @ 479,845: 11x15
#910 @ 726,399: 19x16
#911 @ 257,566: 12x10
#912 @ 735,681: 18x25
#913 @ 196,211: 24x26
#914 @ 511,875: 21x23
#915 @ 167,466: 10x12
#916 @ 725,57: 29x27
#917 @ 234,187: 15x27
#918 @ 848,834: 18x21
#919 @ 482,274: 26x13
#920 @ 760,488: 14x21
#921 @ 188,547: 26x14
#922 @ 731,270: 19x27
#923 @ 435,225: 23x21
#924 @ 831,418: 12x3
#925 @ 264,324: 21x28
#926 @ 223,105: 10x22
#927 @ 646,50: 23x19
#928 @ 640,608: 11x18
#929 @ 219,244: 26x26
#930 @ 960,192: 10x19
#931 @ 721,560: 13x22
#932 @ 888,209: 16x24
#933 @ 914,456: 29x16
#934 @ 95,173: 20x26
#935 @ 284,200: 10x11
#936 @ 395,412: 10x22
#937 @ 266,67: 29x13
#938 @ 136,482: 27x20
#939 @ 136,362: 17x17
#940 @ 298,4: 10x14
#941 @ 39,681: 21x26
#942 @ 431,416: 10x25
#943 @ 902,610: 15x16
#944 @ 36,80: 11x25
#945 @ 543,477: 28x19
#946 @ 701,812: 18x20
#947 @ 15,830: 24x29
#948 @ 168,722: 20x27
#949 @ 900,119: 28x17
#950 @ 692,750: 20x19
#951 @ 307,455: 19x22
#952 @ 347,696: 10x16
#953 @ 280,84: 26x11
#954 @ 533,86: 23x14
#955 @ 312,810: 16x15
#956 @ 32,869: 16x26
#957 @ 915,128: 19x28
#958 @ 580,237: 14x10
#959 @ 327,527: 23x12
#960 @ 581,946: 11x17
#961 @ 746,372: 26x16
#962 @ 588,480: 10x27
#963 @ 345,93: 16x28
#964 @ 646,487: 23x23
#965 @ 406,722: 29x27
#966 @ 594,22: 18x14
#967 @ 434,354: 22x12
#968 @ 66,458: 13x21
#969 @ 798,216: 24x20
#970 @ 279,477: 18x15
#971 @ 922,243: 26x20
#972 @ 546,789: 15x18
#973 @ 730,471: 28x10
#974 @ 209,122: 28x26
#975 @ 268,556: 28x23
#976 @ 687,519: 20x10
#977 @ 526,879: 24x26
#978 @ 814,711: 26x11
#979 @ 453,110: 26x19
#980 @ 298,717: 19x16
#981 @ 768,802: 18x12
#982 @ 325,707: 22x24
#983 @ 108,854: 16x20
#984 @ 541,907: 23x12
#985 @ 328,505: 27x29
#986 @ 217,336: 26x26
#987 @ 84,139: 19x21
#988 @ 183,307: 28x29
#989 @ 51,522: 14x20
#990 @ 565,701: 19x24
#991 @ 104,53: 15x22
#992 @ 178,517: 21x13
#993 @ 571,74: 15x29
#994 @ 658,58: 7x5
#995 @ 583,622: 25x20
#996 @ 758,655: 13x12
#997 @ 238,640: 21x26
#998 @ 142,545: 11x22
#999 @ 601,88: 29x20
#1000 @ 20,329: 10x12
#1001 @ 407,908: 28x24
#1002 @ 451,741: 22x17
#1003 @ 521,509: 10x15
#1004 @ 40,596: 28x29
#1005 @ 357,303: 27x11
#1006 @ 54,164: 11x15
#1007 @ 203,181: 29x28
#1008 @ 608,524: 26x10
#1009 @ 603,553: 14x28
#1010 @ 850,453: 11x15
#1011 @ 643,511: 20x10
#1012 @ 905,723: 25x14
#1013 @ 372,952: 23x11
#1014 @ 763,855: 18x29
#1015 @ 523,400: 16x17
#1016 @ 206,228: 27x16
#1017 @ 382,24: 23x17
#1018 @ 37,535: 25x21
#1019 @ 27,503: 26x29
#1020 @ 390,418: 28x11
#1021 @ 855,79: 17x18
#1022 @ 699,947: 16x26
#1023 @ 174,849: 20x28
#1024 @ 431,195: 12x28
#1025 @ 388,185: 28x22
#1026 @ 332,692: 25x20
#1027 @ 786,50: 27x10
#1028 @ 879,65: 28x12
#1029 @ 587,229: 13x21
#1030 @ 504,280: 19x17
#1031 @ 144,355: 17x27
#1032 @ 167,207: 11x25
#1033 @ 53,212: 12x17
#1034 @ 492,619: 19x20
#1035 @ 24,394: 29x15
#1036 @ 196,844: 29x23
#1037 @ 527,636: 22x23
#1038 @ 510,149: 25x12
#1039 @ 518,422: 17x10
#1040 @ 544,529: 16x29
#1041 @ 339,503: 22x21
#1042 @ 664,203: 18x21
#1043 @ 46,943: 25x25
#1044 @ 329,343: 25x20
#1045 @ 556,845: 16x22
#1046 @ 881,225: 13x21
#1047 @ 122,165: 27x28
#1048 @ 814,127: 12x28
#1049 @ 88,270: 11x15
#1050 @ 426,919: 15x22
#1051 @ 873,50: 17x14
#1052 @ 373,678: 12x15
#1053 @ 74,463: 27x17
#1054 @ 306,255: 18x20
#1055 @ 394,149: 10x11
#1056 @ 530,705: 10x24
#1057 @ 200,455: 23x14
#1058 @ 661,207: 11x13
#1059 @ 120,835: 18x17
#1060 @ 412,767: 24x20
#1061 @ 91,237: 13x25
#1062 @ 866,916: 10x28
#1063 @ 923,115: 18x18
#1064 @ 676,211: 18x11
#1065 @ 856,527: 27x14
#1066 @ 714,511: 17x13
#1067 @ 780,655: 13x19
#1068 @ 518,820: 24x11
#1069 @ 461,75: 17x11
#1070 @ 795,345: 28x10
#1071 @ 619,107: 15x24
#1072 @ 100,887: 17x27
#1073 @ 396,602: 27x22
#1074 @ 149,378: 19x24
#1075 @ 505,876: 23x19
#1076 @ 546,332: 19x29
#1077 @ 426,502: 29x25
#1078 @ 291,956: 10x28
#1079 @ 924,171: 25x17
#1080 @ 168,415: 26x26
#1081 @ 952,207: 22x20
#1082 @ 400,57: 15x24
#1083 @ 866,752: 16x23
#1084 @ 507,189: 20x19
#1085 @ 449,898: 23x14
#1086 @ 72,787: 10x27
#1087 @ 896,69: 27x23
#1088 @ 805,44: 10x18
#1089 @ 651,275: 25x27
#1090 @ 791,563: 16x28
#1091 @ 510,961: 29x27
#1092 @ 716,958: 10x27
#1093 @ 309,514: 23x29
#1094 @ 146,726: 28x17
#1095 @ 880,125: 11x18
#1096 @ 253,966: 14x11
#1097 @ 981,35: 16x15
#1098 @ 325,809: 13x13
#1099 @ 525,96: 16x10
#1100 @ 437,190: 23x23
#1101 @ 849,179: 21x13
#1102 @ 681,969: 10x11
#1103 @ 156,168: 20x11
#1104 @ 514,654: 22x27
#1105 @ 116,570: 29x16
#1106 @ 98,316: 19x29
#1107 @ 651,373: 26x19
#1108 @ 458,555: 18x21
#1109 @ 784,567: 21x15
#1110 @ 581,713: 29x18
#1111 @ 918,521: 27x13
#1112 @ 599,315: 13x14
#1113 @ 884,4: 22x13
#1114 @ 955,898: 14x18
#1115 @ 468,728: 15x19
#1116 @ 589,367: 21x15
#1117 @ 958,891: 13x14
#1118 @ 797,345: 23x20
#1119 @ 696,817: 27x19
#1120 @ 658,443: 14x11
#1121 @ 609,929: 23x28
#1122 @ 234,775: 11x17
#1123 @ 183,795: 18x14
#1124 @ 638,836: 27x20
#1125 @ 95,612: 29x21
#1126 @ 765,708: 22x12
#1127 @ 463,271: 26x21
#1128 @ 85,225: 29x28
#1129 @ 668,431: 28x27
#1130 @ 154,430: 24x24
#1131 @ 475,218: 13x16
#1132 @ 187,493: 28x15
#1133 @ 362,513: 29x21
#1134 @ 617,956: 28x26
#1135 @ 643,756: 21x15
#1136 @ 729,393: 29x23
#1137 @ 305,199: 24x26
#1138 @ 767,847: 12x12
#1139 @ 870,139: 29x14
#1140 @ 210,232: 27x25
#1141 @ 257,834: 26x12
#1142 @ 542,738: 22x20
#1143 @ 592,864: 27x24
#1144 @ 36,918: 16x29
#1145 @ 178,373: 16x19
#1146 @ 233,655: 25x24
#1147 @ 267,600: 26x29
#1148 @ 946,400: 19x17
#1149 @ 643,870: 17x24
#1150 @ 274,479: 20x18
#1151 @ 396,161: 15x17
#1152 @ 644,428: 16x16
#1153 @ 187,412: 17x16
#1154 @ 806,200: 25x20
#1155 @ 228,178: 16x19
#1156 @ 346,140: 29x25
#1157 @ 423,60: 20x15
#1158 @ 731,409: 10x29
#1159 @ 651,373: 17x16
#1160 @ 607,668: 16x15
#1161 @ 673,964: 22x16
#1162 @ 235,490: 28x20
#1163 @ 454,543: 17x16
#1164 @ 625,878: 22x13
#1165 @ 901,353: 17x18
#1166 @ 710,501: 18x26
#1167 @ 917,959: 17x17
#1168 @ 665,9: 21x24
#1169 @ 421,248: 11x28
#1170 @ 733,166: 27x18
#1171 @ 233,547: 23x28
#1172 @ 30,616: 17x16
#1173 @ 40,128: 21x15
#1174 @ 388,166: 25x13
#1175 @ 728,479: 26x10
#1176 @ 147,311: 19x10
#1177 @ 836,234: 24x27
#1178 @ 957,872: 21x20
#1179 @ 594,930: 13x25
#1180 @ 88,849: 26x10
#1181 @ 427,214: 10x18
#1182 @ 883,221: 12x20
#1183 @ 509,495: 23x13
#1184 @ 280,790: 16x10
#1185 @ 400,989: 29x10
#1186 @ 709,655: 26x22
#1187 @ 968,483: 21x16
#1188 @ 246,352: 28x25
#1189 @ 97,175: 15x17
#1190 @ 502,134: 16x28
#1191 @ 591,449: 10x27
#1192 @ 20,762: 24x11
#1193 @ 76,958: 23x23
#1194 @ 628,930: 18x10
#1195 @ 449,782: 14x18
#1196 @ 528,805: 12x24
#1197 @ 587,442: 14x17
#1198 @ 717,296: 28x29
#1199 @ 966,640: 11x20
#1200 @ 725,28: 24x17
#1201 @ 594,7: 13x26
#1202 @ 126,847: 14x10
#1203 @ 747,547: 19x27
#1204 @ 868,438: 28x22
#1205 @ 595,273: 28x17
#1206 @ 314,505: 15x29
#1207 @ 577,265: 19x16
#1208 @ 629,917: 14x16
#1209 @ 227,543: 13x23
#1210 @ 402,481: 12x28
#1211 @ 745,178: 29x19
#1212 @ 772,218: 28x20
#1213 @ 449,172: 21x11
#1214 @ 130,541: 15x14
#1215 @ 116,285: 23x11
#1216 @ 311,501: 13x28
#1217 @ 951,439: 18x17
#1218 @ 786,175: 23x22
#1219 @ 899,967: 24x14
#1220 @ 517,181: 22x29
#1221 @ 945,537: 10x14
#1222 @ 362,477: 13x22
#1223 @ 388,62: 26x14
#1224 @ 614,471: 26x27
#1225 @ 175,737: 26x16
#1226 @ 436,133: 25x23
#1227 @ 674,197: 18x16
#1228 @ 383,23: 11x26
#1229 @ 411,817: 27x17
#1230 @ 940,48: 10x27
#1231 @ 741,802: 28x21
#1232 @ 525,685: 14x23
#1233 @ 323,721: 11x26
#1234 @ 665,268: 27x13
#1235 @ 960,954: 10x16
#1236 @ 918,270: 11x18
#1237 @ 439,238: 18x26
#1238 @ 621,503: 22x16
#1239 @ 2,818: 20x27
#1240 @ 699,203: 24x18
#1241 @ 985,78: 11x21
#1242 @ 976,12: 22x20
#1243 @ 63,148: 11x16
#1244 @ 530,516: 16x20
#1245 @ 906,935: 25x13
#1246 @ 559,956: 25x21
#1247 @ 27,67: 18x13
#1248 @ 403,319: 27x27
#1249 @ 423,268: 27x29
#1250 @ 53,564: 17x22
#1251 @ 454,96: 14x23
#1252 @ 321,660: 12x28
#1253 @ 586,590: 24x22
#1254 @ 699,514: 14x28
#1255 @ 444,481: 27x28
#1256 @ 420,596: 19x14
#1257 @ 345,738: 12x12
#1258 @ 441,325: 10x21
#1259 @ 566,477: 12x10
#1260 @ 689,790: 16x24
#1261 @ 413,446: 28x26
#1262 @ 898,861: 18x18
#1263 @ 342,405: 14x21
#1264 @ 669,782: 10x10
#1265 @ 7,480: 24x12
#1266 @ 686,749: 17x11
#1267 @ 808,709: 16x28
#1268 @ 530,96: 13x21
#1269 @ 876,141: 5x6
#1270 @ 246,130: 23x13
#1271 @ 493,609: 12x10
#1272 @ 900,259: 18x14
#1273 @ 20,492: 10x28
#1274 @ 26,860: 27x24
#1275 @ 369,619: 28x25
#1276 @ 572,457: 27x22
#1277 @ 50,133: 26x12
#1278 @ 620,525: 26x15
#1279 @ 197,585: 29x26
#1280 @ 244,402: 16x13
#1281 @ 423,445: 11x12
#1282 @ 425,178: 11x26
#1283 @ 190,678: 20x13
#1284 @ 229,944: 23x21
#1285 @ 274,615: 22x25
#1286 @ 44,544: 25x27
#1287 @ 634,263: 18x27
#1288 @ 250,65: 25x16
#1289 @ 754,908: 28x26
#1290 @ 120,266: 15x24
#1291 @ 7,287: 10x23
#1292 @ 811,139: 16x27
#1293 @ 168,680: 22x12"""
