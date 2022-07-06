{-# LANGUAGE DataKinds, NegativeLiterals #-}
module GibbsOptBucket where

import           Data.Number.LogFloat hiding (product)
import           Prelude              hiding (product, exp, log, (**))

import           Language.Hakaru.Runtime.LogFloatPrelude
import           Language.Hakaru.Types.Sing
import qualified System.Random.MWC                as MWC
import           Control.Monad

prog = 
  lam $ \ topic_prior0 ->
  lam $ \ word_prior1 ->
  lam $ \ z2 ->
  lam $ \ w3 ->
  lam $ \ doc4 ->
  lam $ \ docUpdate5 ->
  categorical (array (size topic_prior0) $
                     \ zNew丏6 ->
                     product (nat_ 0)
                             (size topic_prior0)
                             (\ i7 ->
                              product (nat_ 0)
                                      (size word_prior1)
                                      (\ i丣8 ->
                                       product (nat_ 0)
                                               (let_ (bucket (nat_ 0)
                                                             (size w3)
                                                             ((r_fanout (r_split (\ (i丙11,()) ->
                                                                                  docUpdate5
                                                                                  == doc4 ! i丙11)
                                                                                 (r_index (\ () ->
                                                                                           size word_prior1)
                                                                                          (\ (i丙11,()) ->
                                                                                           w3
                                                                                           ! i丙11)
                                                                                          (r_add (\ (i丙11,(i丣12,())) ->
                                                                                                  nat_ 1)))
                                                                                 r_nop)
                                                                        r_nop))) $ \ summary10 ->
                                                case_ (i7 == zNew丏6)
                                                      [branch ptrue
                                                              (case_ (case_ summary10
                                                                            [branch (ppair PVar
                                                                                           PVar)
                                                                                    (\ y13 z14 ->
                                                                                     y13)])
                                                                     [branch (ppair PVar PVar)
                                                                             (\ y15 z16 -> y15)]
                                                               ! i丣8),
                                                       branch pfalse (nat_ 0)])
                                               (\ j9 ->
                                                nat2prob (let_ (bucket (nat_ 0)
                                                                       (size w3)
                                                                       ((r_split (\ (i丙18,()) ->
                                                                                  doc4 ! i丙18
                                                                                  == docUpdate5)
                                                                                 r_nop
                                                                                 (r_index (\ () ->
                                                                                           size topic_prior0)
                                                                                          (\ (i丙18,()) ->
                                                                                           z2
                                                                                           ! (doc4
                                                                                              ! i丙18))
                                                                                          (r_index (\ (i19,()) ->
                                                                                                    size word_prior1)
                                                                                                   (\ (i丙18,(i19,())) ->
                                                                                                    w3
                                                                                                    ! i丙18)
                                                                                                   (r_add (\ (i丙18,(i丣20,(i19,()))) ->
                                                                                                           nat_ 1))))))) $ \ summary17 ->
                                                          case_ summary17
                                                                [branch (ppair PVar PVar)
                                                                        (\ y21 z22 -> z22)]
                                                          ! i7
                                                          ! i丣8) +
                                                nat2prob j9 +
                                                word_prior1 ! i丣8))) *
                     ((let_ (bucket (nat_ 0)
                                    (size z2)
                                    ((r_split (\ (i丙24,()) -> i丙24 == docUpdate5)
                                              r_nop
                                              (r_index (\ () -> size topic_prior0)
                                                       (\ (i丙24,()) -> z2 ! i丙24)
                                                       (r_add (\ (i丙24,(zNew丏25,())) ->
                                                               nat_ 1)))))) $ \ summary23 ->
                       nat2prob (case_ summary23
                                       [branch (ppair PVar PVar) (\ y26 z27 -> z27)]
                                 ! zNew丏6)) +
                      topic_prior0 ! zNew丏6) *
                     recip (nat2prob (summate (nat_ 0)
                                              (size z2)
                                              (\ i丙28 ->
                                               case_ (i丙28 == docUpdate5)
                                                     [branch ptrue (nat_ 0),
                                                      branch pfalse
                                                             (case_ (z2 ! i丙28 < nat_ 0)
                                                                    [branch ptrue (nat_ 0),
                                                                     branch pfalse (nat_ 1)])])) +
                            summate (nat_ 0)
                                    (size topic_prior0)
                                    (\ i丙29 -> topic_prior0 ! i丙29)) *
                     recip (product (nat_ 0)
                                    (size topic_prior0)
                                    (\ i30 ->
                                     product (nat_ 0)
                                             (let_ (bucket (nat_ 0)
                                                           (size w3)
                                                           ((r_fanout (r_split (\ (i丙33,()) ->
                                                                                w3 ! i丙33
                                                                                < nat_ 0)
                                                                               r_nop
                                                                               (r_split (\ (i丙33,()) ->
                                                                                         docUpdate5
                                                                                         == doc4
                                                                                            ! i丙33)
                                                                                        (r_add (\ (i丙33,()) ->
                                                                                                nat_ 1))
                                                                                        r_nop))
                                                                      r_nop))) $ \ summary32 ->
                                              case_ (i30 == zNew丏6)
                                                    [branch ptrue
                                                            (case_ (case_ (case_ summary32
                                                                                 [branch (ppair PVar
                                                                                                PVar)
                                                                                         (\ y34
                                                                                            z35 ->
                                                                                          y34)])
                                                                          [branch (ppair PVar PVar)
                                                                                  (\ y36 z37 ->
                                                                                   z37)])
                                                                   [branch (ppair PVar PVar)
                                                                           (\ y38 z39 -> y38)]),
                                                     branch pfalse (nat_ 0)])
                                             (\ i丣31 ->
                                              nat2prob (let_ (bucket (nat_ 0)
                                                                     (size w3)
                                                                     ((r_split (\ (i丙41,()) ->
                                                                                w3 ! i丙41
                                                                                < nat_ 0)
                                                                               r_nop
                                                                               (r_split (\ (i丙41,()) ->
                                                                                         doc4 ! i丙41
                                                                                         == docUpdate5)
                                                                                        r_nop
                                                                                        (r_index (\ () ->
                                                                                                  size topic_prior0)
                                                                                                 (\ (i丙41,()) ->
                                                                                                  z2
                                                                                                  ! (doc4
                                                                                                     ! i丙41))
                                                                                                 (r_add (\ (i丙41,(i42,())) ->
                                                                                                         nat_ 1))))))) $ \ summary40 ->
                                                        case_ (case_ summary40
                                                                     [branch (ppair PVar PVar)
                                                                             (\ y43 z44 -> z44)])
                                                              [branch (ppair PVar PVar)
                                                                      (\ y45 z46 -> z46)]
                                                        ! i30) +
                                              nat2prob i丣31 +
                                              summate (nat_ 0)
                                                      (size word_prior1)
                                                      (\ i丙47 -> word_prior1 ! i丙47)))))
