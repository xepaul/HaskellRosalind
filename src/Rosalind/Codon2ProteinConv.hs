{-# LANGUAGE ImportQualifiedPost #-}
module Rosalind.Codon2ProteinConv where


import Rosalind.RnaBase (RnaBase(..))
import Rosalind.ProteinWithStop qualified as P (ProteinWithStop(..))
import Rosalind.ProteinWithStop (ProteinWithStop)

rdaCodon2ProteinWithStop :: (RnaBase,RnaBase,RnaBase) -> ProteinWithStop
rdaCodon2ProteinWithStop (U,U,U) = P.F
rdaCodon2ProteinWithStop (U,U,C) = P.F
rdaCodon2ProteinWithStop (U,U,A) = P.L
rdaCodon2ProteinWithStop (U,U,G) = P.L
rdaCodon2ProteinWithStop (U,C,U) = P.S
rdaCodon2ProteinWithStop (U,C,C) = P.S
rdaCodon2ProteinWithStop (U,C,A) = P.S
rdaCodon2ProteinWithStop (U,C,G) = P.S
rdaCodon2ProteinWithStop (U,A,U) = P.Y
rdaCodon2ProteinWithStop (U,A,C) = P.Y
rdaCodon2ProteinWithStop (U,A,A) = P.Stop
rdaCodon2ProteinWithStop (U,A,G) = P.Stop
rdaCodon2ProteinWithStop (U,G,U) = P.C
rdaCodon2ProteinWithStop (U,G,C) = P.C
rdaCodon2ProteinWithStop (U,G,A) = P.Stop
rdaCodon2ProteinWithStop (U,G,G) = P.W
rdaCodon2ProteinWithStop (C,U,U) = P.L
rdaCodon2ProteinWithStop (C,U,C) = P.L
rdaCodon2ProteinWithStop (C,U,A) = P.L
rdaCodon2ProteinWithStop (C,U,G) = P.L
rdaCodon2ProteinWithStop (C,C,U) = P.P
rdaCodon2ProteinWithStop (C,C,C) = P.P
rdaCodon2ProteinWithStop (C,C,A) = P.P
rdaCodon2ProteinWithStop (C,C,G) = P.P
rdaCodon2ProteinWithStop (C,A,U) = P.H
rdaCodon2ProteinWithStop (C,A,C) = P.H
rdaCodon2ProteinWithStop (C,A,A) = P.Q
rdaCodon2ProteinWithStop (C,A,G) = P.Q
rdaCodon2ProteinWithStop (C,G,U) = P.R
rdaCodon2ProteinWithStop (C,G,C) = P.R
rdaCodon2ProteinWithStop (C,G,A) = P.R
rdaCodon2ProteinWithStop (C,G,G) = P.R
rdaCodon2ProteinWithStop (A,U,U) = P.I
rdaCodon2ProteinWithStop (A,U,C) = P.I
rdaCodon2ProteinWithStop (A,U,A) = P.I
rdaCodon2ProteinWithStop (A,U,G) = P.M
rdaCodon2ProteinWithStop (A,C,U) = P.T
rdaCodon2ProteinWithStop (A,C,C) = P.T
rdaCodon2ProteinWithStop (A,C,A) = P.T
rdaCodon2ProteinWithStop (A,C,G) = P.T
rdaCodon2ProteinWithStop (A,A,U) = P.N
rdaCodon2ProteinWithStop (A,A,C) = P.N
rdaCodon2ProteinWithStop (A,A,A) = P.K
rdaCodon2ProteinWithStop (A,A,G) = P.K
rdaCodon2ProteinWithStop (A,G,U) = P.S
rdaCodon2ProteinWithStop (A,G,C) = P.S
rdaCodon2ProteinWithStop (A,G,A) = P.R
rdaCodon2ProteinWithStop (A,G,G) = P.R
rdaCodon2ProteinWithStop (G,U,U) = P.V
rdaCodon2ProteinWithStop (G,U,C) = P.V
rdaCodon2ProteinWithStop (G,U,A) = P.V
rdaCodon2ProteinWithStop (G,U,G) = P.V
rdaCodon2ProteinWithStop (G,C,U) = P.A
rdaCodon2ProteinWithStop (G,C,C) = P.A
rdaCodon2ProteinWithStop (G,C,A) = P.A
rdaCodon2ProteinWithStop (G,C,G) = P.A
rdaCodon2ProteinWithStop (G,A,U) = P.D
rdaCodon2ProteinWithStop (G,A,C) = P.D
rdaCodon2ProteinWithStop (G,A,A) = P.E
rdaCodon2ProteinWithStop (G,A,G) = P.E
rdaCodon2ProteinWithStop (G,G,U) = P.G
rdaCodon2ProteinWithStop (G,G,C) = P.G
rdaCodon2ProteinWithStop (G,G,A) = P.G
rdaCodon2ProteinWithStop (G,G,G) = P.G