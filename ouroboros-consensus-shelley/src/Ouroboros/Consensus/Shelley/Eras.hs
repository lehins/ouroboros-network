{-# LANGUAGE DataKinds               #-}
{-# LANGUAGE EmptyCase               #-}
{-# LANGUAGE FlexibleContexts        #-}
{-# LANGUAGE FlexibleInstances       #-}
{-# LANGUAGE LambdaCase              #-}
{-# LANGUAGE MultiParamTypeClasses   #-}
{-# LANGUAGE OverloadedStrings       #-}
{-# LANGUAGE ScopedTypeVariables     #-}
{-# LANGUAGE TypeApplications        #-}
{-# LANGUAGE TypeFamilies            #-}
{-# LANGUAGE UndecidableInstances    #-}
{-# LANGUAGE UndecidableSuperClasses #-}

module Ouroboros.Consensus.Shelley.Eras (
    -- * Eras based on the Shelley ledger
    AllegraEra
  , AlonzoEra
  , MaryEra
  , ShelleyEra
    -- * Eras instantiated with standard crypto
  , StandardAllegra
  , StandardAlonzo
  , StandardMary
  , StandardShelley
    -- * Shelley-based era
  , ShelleyBasedEra (..)
  , WrapTxInBlock (..)
    -- ** Alonzo era
  , AlonzoApplyTxError (..)
  , AlonzoApplyTxWarning (..)
    -- * Type synonyms for convenience
  , EraCrypto
    -- * Re-exports
  , StandardCrypto
  ) where

import qualified Codec.CBOR.Decoding as Dec
import qualified Codec.CBOR.Encoding as Enc
import           Control.Monad.Except
import           Data.Default.Class (Default)
import           Data.Proxy (Proxy (..))
import           Data.Text (Text)
import           Data.Typeable (Typeable)
import           Data.Void (Void)
import           GHC.Records
import           NoThunks.Class (NoThunks)
import           Numeric.Natural (Natural)

import           Cardano.Binary (FromCBOR (..), ToCBOR (..), enforceSize)

import           Cardano.Ledger.Allegra (AllegraEra)
import           Cardano.Ledger.Allegra.Translation ()
import           Cardano.Ledger.Alonzo (AlonzoEra)
import qualified Cardano.Ledger.Alonzo.PParams as Alonzo
import qualified Cardano.Ledger.Alonzo.Translation as Alonzo
import qualified Cardano.Ledger.Alonzo.Tx as Alonzo
import           Cardano.Ledger.BaseTypes
import qualified Cardano.Ledger.Core as Core
import           Cardano.Ledger.Crypto (StandardCrypto)
import           Cardano.Ledger.Era (Crypto, SupportsSegWit (..))
import qualified Cardano.Ledger.Era as Core
import           Cardano.Ledger.Mary (MaryEra)
import           Cardano.Ledger.Mary.Translation ()
import           Cardano.Ledger.Serialization
import           Cardano.Ledger.Shelley (ShelleyEra)
import           Cardano.Ledger.ShelleyMA ()
import           Control.State.Transition (State)
import qualified Shelley.Spec.Ledger.API as SL

import           Ouroboros.Consensus.Ledger.SupportsMempool
                     (WhetherToIntervene (..))
import           Ouroboros.Consensus.Util (ShowProxy (..))

{-------------------------------------------------------------------------------
  Eras instantiated with standard crypto
-------------------------------------------------------------------------------}

-- | The Shelley era with standard crypto
type StandardShelley = ShelleyEra StandardCrypto

-- | The Allegra era with standard crypto
type StandardAllegra = AllegraEra StandardCrypto

-- | The Mary era with standard crypto
type StandardMary = MaryEra StandardCrypto

-- | The Alonzo era with standard crypto
type StandardAlonzo = AlonzoEra StandardCrypto

{-------------------------------------------------------------------------------
  Type synonyms for convenience
-------------------------------------------------------------------------------}

-- | The 'Cardano.Ledger.Era.Crypto' type family conflicts with the
-- 'Cardano.Ledger.Crypto.Crypto' class. To avoid having to import one or both
-- of them qualified, define 'EraCrypto' as an alias of the former: /return the
-- crypto used by this era/.
type EraCrypto era = Crypto era

{-------------------------------------------------------------------------------
  Era polymorphism
-------------------------------------------------------------------------------}

-- | The ledger already defines 'SL.ShelleyBasedEra' as /the/ top-level
-- constraint on an era, however, consensus often needs some more functionality
-- than the ledger currently provides.
--
-- Either the functionality shouldn't or can't live in the ledger, in which case
-- it can be part and remain part of 'ShelleyBasedEra'. Or, the functionality
-- /should/ live in the ledger, but hasn't yet been added to the ledger, or it
-- hasn't yet been propagated to this repository, in which case it can be added
-- to this class until that is the case.
--
-- By having the same name as the class defined in ledger, we can, if this class
-- becomes redundant, switch to the ledger-defined one without having to update
-- all the code using it. We can just export the right one from this module.
--
-- TODO Currently we include some constraints on the update state which are
-- needed to determine the hard fork point. In the future this should be
-- replaced with an appropriate API - see
-- https://github.com/input-output-hk/ouroboros-network/issues/2890
class ( SL.ShelleyBasedEra era

      , State (Core.EraRule "PPUP" era) ~ SL.PPUPState era
      , Default (State (Core.EraRule "PPUP" era))

      , HasField "_maxBHSize" (Core.PParams era) Natural
      , HasField "_maxTxSize" (Core.PParams era) Natural
      , HasField "_a0" (Core.PParams era) Rational
      , HasField "_nOpt" (Core.PParams era) Natural
      , HasField "_rho" (Core.PParams era) UnitInterval
      , HasField "_tau" (Core.PParams era) UnitInterval

      , FromCBOR (Core.PParams era)
      , ToCBOR (Core.PParams era)

      , HasField "_protocolVersion" (Core.PParamsDelta era) (SL.StrictMaybe SL.ProtVer)
      , FromCBOR (Core.PParamsDelta era)

      , SL.AdditionalGenesisConfig era ~ Core.TranslationContext era
      , ToCBORGroup (TxSeq era)

      , Eq (Core.TxInBlock era)
      , NoThunks (Core.TxInBlock era)
      , Show (Core.TxInBlock era)

      , NoThunks (Core.TranslationContext era)

      , ToCBOR (Core.Witnesses era)

      , Eq (ShelleyApplyTxError era)
      , FromCBOR (ShelleyApplyTxError era)
      , Show (ShelleyApplyTxError era)
      , ShowProxy (ShelleyApplyTxError era)
      , ToCBOR (ShelleyApplyTxError era)
      ) => ShelleyBasedEra era where
  type ShelleyApplyTxError era

  -- | Return the name of the Shelley-based era, e.g., @"Shelley"@, @"Allegra"@,
  -- etc.
  shelleyBasedEraName :: proxy era -> Text

  toShelleyApplyTxError :: proxy era -> SL.ApplyTxError era -> ShelleyApplyTxError era

  applyShelleyBasedTx ::
       SL.Globals
    -> SL.LedgerEnv    era
    -> SL.MempoolState era
    -> WhetherToIntervene
    -> SL.Tx           era
    -> Except
         (ShelleyApplyTxError era)
         ( SL.MempoolState era
         , TxInBlock       era
         )

-- | The default implementation of 'applyShelleyBasedTx', a thin wrapper around
-- 'SL.applyTx'
defaultApplyShelleyBasedTx :: forall era.
     ShelleyBasedEra era
  => SL.Globals
  -> SL.LedgerEnv    era
  -> SL.MempoolState era
  -> WhetherToIntervene
  -> SL.Tx           era
  -> Except
       (ShelleyApplyTxError era)
       ( SL.MempoolState era
       , TxInBlock       era
       )
defaultApplyShelleyBasedTx globals ledgerEnv mempoolState _wti tx =
      withExcept (toShelleyApplyTxError (Proxy @era))
    $ SL.applyTx
        globals
        ledgerEnv
        mempoolState
        tx

instance (SL.PraosCrypto c, ShowProxy (SL.ApplyTxError (ShelleyEra c))) => ShelleyBasedEra (ShelleyEra c) where
  type ShelleyApplyTxError (ShelleyEra c) = SL.ApplyTxError (ShelleyEra c)

  shelleyBasedEraName _ = "Shelley"

  toShelleyApplyTxError _prx = id

  applyShelleyBasedTx = defaultApplyShelleyBasedTx

instance (SL.PraosCrypto c, ShowProxy (SL.ApplyTxError (AllegraEra c))) => ShelleyBasedEra (AllegraEra c) where
  type ShelleyApplyTxError (AllegraEra c) = SL.ApplyTxError (AllegraEra c)

  shelleyBasedEraName _ = "Allegra"

  toShelleyApplyTxError _prx = id

  applyShelleyBasedTx = defaultApplyShelleyBasedTx

instance (SL.PraosCrypto c, ShowProxy (SL.ApplyTxError (MaryEra c))) => ShelleyBasedEra (MaryEra c) where
  type ShelleyApplyTxError (MaryEra c) = SL.ApplyTxError (MaryEra c)

  shelleyBasedEraName _ = "Mary"

  toShelleyApplyTxError _prx = id

  applyShelleyBasedTx = defaultApplyShelleyBasedTx

instance SL.PraosCrypto c => ShelleyBasedEra (AlonzoEra c) where
  type ShelleyApplyTxError (AlonzoEra c) = AlonzoApplyTxError c

  shelleyBasedEraName _ = "Alonzo"

  toShelleyApplyTxError _prx = AlonzoApplyTxErrorProper

  applyShelleyBasedTx globals ledgerEnv mempoolState wti tx = do
      (mempoolState', vtx) <- defaultApplyShelleyBasedTx
        globals
        ledgerEnv
        mempoolState
        wti
        tx

      let Alonzo.IsValidating scriptsOK = Alonzo.isValidating vtx
      case wti of
        Intervene | not scriptsOK ->
            throwError
          $ AlonzoApplyTxErrorPromotedWarning
          $ AlonzoPlutusErrors []
        _ -> pure ()

      pure (mempoolState', vtx)

-- | An /error/ arising from applying an Alonzo transaction
data AlonzoApplyTxError c
  -- | The transaction is invalid
  = AlonzoApplyTxErrorProper          !(SL.ApplyTxError (AlonzoEra c))
  -- | The transaction is valid, but it has warnings that were promoted to
  -- errors as part of an intervention
  --
  -- See 'WhetherToIntervene'.
  | AlonzoApplyTxErrorPromotedWarning !AlonzoApplyTxWarning
  deriving (Eq, Show)

instance Typeable c => ShowProxy (AlonzoApplyTxError c) where

instance SL.PraosCrypto c => ToCBOR (AlonzoApplyTxError c) where
  toCBOR = \case
    AlonzoApplyTxErrorProper err -> mconcat [
        Enc.encodeListLen 2
      , Enc.encodeWord8 0
      , toCBOR err
      ]
    AlonzoApplyTxErrorPromotedWarning warn -> mconcat [
        Enc.encodeListLen 2
      , Enc.encodeWord8 1
      , toCBOR warn
      ]

instance SL.PraosCrypto c => FromCBOR (AlonzoApplyTxError c) where
  fromCBOR = do
    enforceSize "AlonzoApplyTxError" 2
    Dec.decodeWord8 >>= \case
      0   -> AlonzoApplyTxErrorProper          <$> fromCBOR
      1   -> AlonzoApplyTxErrorPromotedWarning <$> fromCBOR
      tag -> fail $  "fromCBOR @AlonzoApplyTxError: unexpected tag " <> show tag

-- | A /warning/ arising from applying an Alonzo transaction
data AlonzoApplyTxWarning
    -- | TODO eventually the Alonzo ledger interface will provide details of
    -- the Plutus failure; Void for now ensures the empty list
  = AlonzoPlutusErrors [Void]
  deriving (Eq, Show)

instance ToCBOR AlonzoApplyTxWarning where
  toCBOR = \case
    AlonzoPlutusErrors errs -> mconcat [
        Enc.encodeListLen 2
      , Enc.encodeWord8 0
      , toCBOR errs
      ]

instance FromCBOR AlonzoApplyTxWarning where
  fromCBOR = do
    enforceSize "AlonzoApplyTxWarning" 2
    Dec.decodeWord8 >>= \case
      0   -> AlonzoPlutusErrors <$> fromCBOR
      tag -> fail $  "fromCBOR @AlonzoApplyTxWarning: unexpected tag " <> show tag

{-------------------------------------------------------------------------------
  TxInBlock wrapper
-------------------------------------------------------------------------------}

-- | Wrapper for partially applying 'Core.TxInBlock'
--
-- For generality, Consensus uses that type family as eg the index of
-- 'Core.TranslateEra'. We thus need to partially apply it.
--
-- @cardano-ledger-specs@ also declares such a newtype, but currently it's only
-- defined in the Alonzo translation module, which seems somewhat inappropriate
-- to use for previous eras. Also, we use a @Wrap@ prefix in Consensus. Hence
-- this minor mediating definition. TODO I'm not even fully persuading myself
-- with this justification.
newtype WrapTxInBlock era = WrapTxInBlock {unwrapTxInBlock :: Core.TxInBlock era}

instance ShelleyBasedEra (AllegraEra c) => Core.TranslateEra (AllegraEra c) WrapTxInBlock where
  type TranslationError (AllegraEra c) WrapTxInBlock = Core.TranslationError (AllegraEra c) SL.Tx
  translateEra ctxt = fmap WrapTxInBlock . Core.translateEra ctxt . unwrapTxInBlock

instance ShelleyBasedEra (MaryEra c) => Core.TranslateEra (MaryEra c) WrapTxInBlock where
  type TranslationError (MaryEra c) WrapTxInBlock = Core.TranslationError (MaryEra c) SL.Tx
  translateEra ctxt = fmap WrapTxInBlock . Core.translateEra ctxt . unwrapTxInBlock

instance ShelleyBasedEra (AlonzoEra c) => Core.TranslateEra (AlonzoEra c) WrapTxInBlock where
  type TranslationError (AlonzoEra c) WrapTxInBlock = Core.TranslationError (AlonzoEra c) Alonzo.TxInBlock
  translateEra ctxt =
        fmap (\(Alonzo.TxInBlock tx) -> WrapTxInBlock tx)
      . Core.translateEra @(AlonzoEra c) ctxt
      . Alonzo.TxInBlock . unwrapTxInBlock
