{-# LANGUAGE ImportQualifiedPost #-}
module Rosalind.Codon2ProteinConv where

import Rosalind.RnaBase (RnaBase(..))
import Rosalind.ProteinWithStop qualified as P (ProteinWithStop(..))
import Rosalind.ProteinWithStop (ProteinWithStop)

rnaCodon2ProteinWithStop :: (RnaBase,RnaBase,RnaBase) -> ProteinWithStop
rnaCodon2ProteinWithStop (U,U,U) = P.F
rnaCodon2ProteinWithStop (U,U,C) = P.F
rnaCodon2ProteinWithStop (U,U,A) = P.L
rnaCodon2ProteinWithStop (U,U,G) = P.L
rnaCodon2ProteinWithStop (U,C,U) = P.S
rnaCodon2ProteinWithStop (U,C,C) = P.S
rnaCodon2ProteinWithStop (U,C,A) = P.S
rnaCodon2ProteinWithStop (U,C,G) = P.S
rnaCodon2ProteinWithStop (U,A,U) = P.Y
rnaCodon2ProteinWithStop (U,A,C) = P.Y
rnaCodon2ProteinWithStop (U,A,A) = P.Stop
rnaCodon2ProteinWithStop (U,A,G) = P.Stop
rnaCodon2ProteinWithStop (U,G,U) = P.C
rnaCodon2ProteinWithStop (U,G,C) = P.C
rnaCodon2ProteinWithStop (U,G,A) = P.Stop
rnaCodon2ProteinWithStop (U,G,G) = P.W
rnaCodon2ProteinWithStop (C,U,U) = P.L
rnaCodon2ProteinWithStop (C,U,C) = P.L
rnaCodon2ProteinWithStop (C,U,A) = P.L
rnaCodon2ProteinWithStop (C,U,G) = P.L
rnaCodon2ProteinWithStop (C,C,U) = P.P
rnaCodon2ProteinWithStop (C,C,C) = P.P
rnaCodon2ProteinWithStop (C,C,A) = P.P
rnaCodon2ProteinWithStop (C,C,G) = P.P
rnaCodon2ProteinWithStop (C,A,U) = P.H
rnaCodon2ProteinWithStop (C,A,C) = P.H
rnaCodon2ProteinWithStop (C,A,A) = P.Q
rnaCodon2ProteinWithStop (C,A,G) = P.Q
rnaCodon2ProteinWithStop (C,G,U) = P.R
rnaCodon2ProteinWithStop (C,G,C) = P.R
rnaCodon2ProteinWithStop (C,G,A) = P.R
rnaCodon2ProteinWithStop (C,G,G) = P.R
rnaCodon2ProteinWithStop (A,U,U) = P.I
rnaCodon2ProteinWithStop (A,U,C) = P.I
rnaCodon2ProteinWithStop (A,U,A) = P.I
rnaCodon2ProteinWithStop (A,U,G) = P.M
rnaCodon2ProteinWithStop (A,C,U) = P.T
rnaCodon2ProteinWithStop (A,C,C) = P.T
rnaCodon2ProteinWithStop (A,C,A) = P.T
rnaCodon2ProteinWithStop (A,C,G) = P.T
rnaCodon2ProteinWithStop (A,A,U) = P.N
rnaCodon2ProteinWithStop (A,A,C) = P.N
rnaCodon2ProteinWithStop (A,A,A) = P.K
rnaCodon2ProteinWithStop (A,A,G) = P.K
rnaCodon2ProteinWithStop (A,G,U) = P.S
rnaCodon2ProteinWithStop (A,G,C) = P.S
rnaCodon2ProteinWithStop (A,G,A) = P.R
rnaCodon2ProteinWithStop (A,G,G) = P.R
rnaCodon2ProteinWithStop (G,U,U) = P.V
rnaCodon2ProteinWithStop (G,U,C) = P.V
rnaCodon2ProteinWithStop (G,U,A) = P.V
rnaCodon2ProteinWithStop (G,U,G) = P.V
rnaCodon2ProteinWithStop (G,C,U) = P.A
rnaCodon2ProteinWithStop (G,C,C) = P.A
rnaCodon2ProteinWithStop (G,C,A) = P.A
rnaCodon2ProteinWithStop (G,C,G) = P.A
rnaCodon2ProteinWithStop (G,A,U) = P.D
rnaCodon2ProteinWithStop (G,A,C) = P.D
rnaCodon2ProteinWithStop (G,A,A) = P.E
rnaCodon2ProteinWithStop (G,A,G) = P.E
rnaCodon2ProteinWithStop (G,G,U) = P.G
rnaCodon2ProteinWithStop (G,G,C) = P.G
rnaCodon2ProteinWithStop (G,G,A) = P.G
rnaCodon2ProteinWithStop (G,G,G) = P.G