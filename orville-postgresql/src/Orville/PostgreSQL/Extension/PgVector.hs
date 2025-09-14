{- |
Copyright : Flipstone Technology Partners 2025
License   : MIT
Stability : Stable

Functionality for loading and using the
[pgvector](https://github.com/pgvector/pgvector) extension. This extension provides
vector similarity search capabilities for PostgreSQL.

@since 1.1.0.0
-}
module Orville.PostgreSQL.Extension.PgVector
  ( -- * Vector data types
    FieldDefinition.vectorField
  , FieldDefinition.vectorFieldWithDimension
  , SqlType.vector
  , SqlType.vectorWithDimension

    -- * Distance operators
  , l2Distance
  , negativeInnerProduct
  , cosineDistance
  , l1Distance
  , hammingDistance
  , jaccardDistance

    -- * Distance operator expressions
  , l2DistanceExpr
  , negativeInnerProductExpr
  , cosineDistanceExpr
  , l1DistanceExpr
  , hammingDistanceExpr
  , jaccardDistanceExpr

    -- * Synthetic fields for distance calculations
  , l2DistanceSyntheticField
  , negativeInnerProductSyntheticField
  , cosineDistanceSyntheticField
  , l1DistanceSyntheticField
  , hammingDistanceSyntheticField
  , jaccardDistanceSyntheticField

    -- * Vector indexes
  , mkNamedVectorHnswIndexDefinition
  , mkNamedVectorIvfflatIndexDefinition
  ) where

import qualified Orville.PostgreSQL.Expr as Expr
import qualified Orville.PostgreSQL.Internal.IndexDefinition as IndexDefinition
import qualified Orville.PostgreSQL.Marshall as Marshall
import qualified Orville.PostgreSQL.Marshall.FieldDefinition as FieldDefinition
import qualified Orville.PostgreSQL.Marshall.SqlType as SqlType
import qualified Orville.PostgreSQL.Raw.RawSql as RawSql

-- | L2 distance operator (<->)
l2Distance ::
  Expr.ValueExpression ->
  Expr.ValueExpression ->
  Expr.ValueExpression
l2Distance left right = l2DistanceExpr left right

-- | L2 distance operator expression
l2DistanceExpr ::
  Expr.ValueExpression ->
  Expr.ValueExpression ->
  Expr.ValueExpression
l2DistanceExpr =
  Expr.binaryOpExpression (Expr.binaryOperator "<->")

-- | Negative inner product operator (<#>)
negativeInnerProduct ::
  Expr.ValueExpression ->
  Expr.ValueExpression ->
  Expr.ValueExpression
negativeInnerProduct left right = negativeInnerProductExpr left right

-- | Negative inner product operator expression
negativeInnerProductExpr ::
  Expr.ValueExpression ->
  Expr.ValueExpression ->
  Expr.ValueExpression
negativeInnerProductExpr =
  Expr.binaryOpExpression (Expr.binaryOperator "<#>")

-- | Cosine distance operator (<=>)
cosineDistance ::
  Expr.ValueExpression ->
  Expr.ValueExpression ->
  Expr.ValueExpression
cosineDistance left right = cosineDistanceExpr left right

-- | Cosine distance operator expression
cosineDistanceExpr ::
  Expr.ValueExpression ->
  Expr.ValueExpression ->
  Expr.ValueExpression
cosineDistanceExpr =
  Expr.binaryOpExpression (Expr.binaryOperator "<=>")

-- | L1 distance operator (<+>)
l1Distance ::
  Expr.ValueExpression ->
  Expr.ValueExpression ->
  Expr.ValueExpression
l1Distance left right = l1DistanceExpr left right

-- | L1 distance operator expression
l1DistanceExpr ::
  Expr.ValueExpression ->
  Expr.ValueExpression ->
  Expr.ValueExpression
l1DistanceExpr =
  Expr.binaryOpExpression (Expr.binaryOperator "<+>")

-- | Hamming distance operator (<~>)
hammingDistance ::
  Expr.ValueExpression ->
  Expr.ValueExpression ->
  Expr.ValueExpression
hammingDistance left right = hammingDistanceExpr left right

-- | Hamming distance operator expression
hammingDistanceExpr ::
  Expr.ValueExpression ->
  Expr.ValueExpression ->
  Expr.ValueExpression
hammingDistanceExpr =
  Expr.binaryOpExpression (Expr.binaryOperator "<~>")

-- | Jaccard distance operator (<%>)
jaccardDistance ::
  Expr.ValueExpression ->
  Expr.ValueExpression ->
  Expr.ValueExpression
jaccardDistance left right = jaccardDistanceExpr left right

-- | Jaccard distance operator expression
jaccardDistanceExpr ::
  Expr.ValueExpression ->
  Expr.ValueExpression ->
  Expr.ValueExpression
jaccardDistanceExpr =
  Expr.binaryOpExpression (Expr.binaryOperator "<%>")

-- | Create a synthetic field for L2 distance calculation
l2DistanceSyntheticField ::
  -- | The column to be used in the distance comparison
  Expr.ColumnName ->
  -- | The value to be compared against
  Expr.ValueExpression ->
  -- | The alias to be used to name the distance result
  String ->
  -- | A field with the resulting L2 distance
  Marshall.SyntheticField Double
l2DistanceSyntheticField colname compareVal fieldAlias =
  Marshall.syntheticField
    (l2Distance (Expr.columnReference $ Expr.unqualified colname) compareVal)
    fieldAlias
    Marshall.double

-- | Create a synthetic field for negative inner product calculation
negativeInnerProductSyntheticField ::
  -- | The column to be used in the distance comparison
  Expr.ColumnName ->
  -- | The value to be compared against
  Expr.ValueExpression ->
  -- | The alias to be used to name the distance result
  String ->
  -- | A field with the resulting negative inner product
  Marshall.SyntheticField Double
negativeInnerProductSyntheticField colname compareVal fieldAlias =
  Marshall.syntheticField
    (negativeInnerProduct (Expr.columnReference $ Expr.unqualified colname) compareVal)
    fieldAlias
    Marshall.double

-- | Create a synthetic field for cosine distance calculation
cosineDistanceSyntheticField ::
  -- | The column to be used in the distance comparison
  Expr.ColumnName ->
  -- | The value to be compared against
  Expr.ValueExpression ->
  -- | The alias to be used to name the distance result
  String ->
  -- | A field with the resulting cosine distance
  Marshall.SyntheticField Double
cosineDistanceSyntheticField colname compareVal fieldAlias =
  Marshall.syntheticField
    (cosineDistance (Expr.columnReference $ Expr.unqualified colname) compareVal)
    fieldAlias
    Marshall.double

-- | Create a synthetic field for L1 distance calculation
l1DistanceSyntheticField ::
  -- | The column to be used in the distance comparison
  Expr.ColumnName ->
  -- | The value to be compared against
  Expr.ValueExpression ->
  -- | The alias to be used to name the distance result
  String ->
  -- | A field with the resulting L1 distance
  Marshall.SyntheticField Double
l1DistanceSyntheticField colname compareVal fieldAlias =
  Marshall.syntheticField
    (l1Distance (Expr.columnReference $ Expr.unqualified colname) compareVal)
    fieldAlias
    Marshall.double

-- | Create a synthetic field for Hamming distance calculation
hammingDistanceSyntheticField ::
  -- | The column to be used in the distance comparison
  Expr.ColumnName ->
  -- | The value to be compared against
  Expr.ValueExpression ->
  -- | The alias to be used to name the distance result
  String ->
  -- | A field with the resulting Hamming distance
  Marshall.SyntheticField Double
hammingDistanceSyntheticField colname compareVal fieldAlias =
  Marshall.syntheticField
    (hammingDistance (Expr.columnReference $ Expr.unqualified colname) compareVal)
    fieldAlias
    Marshall.double

-- | Create a synthetic field for Jaccard distance calculation
jaccardDistanceSyntheticField ::
  -- | The column to be used in the distance comparison
  Expr.ColumnName ->
  -- | The value to be compared against
  Expr.ValueExpression ->
  -- | The alias to be used to name the distance result
  String ->
  -- | A field with the resulting Jaccard distance
  Marshall.SyntheticField Double
jaccardDistanceSyntheticField colname compareVal fieldAlias =
  Marshall.syntheticField
    (jaccardDistance (Expr.columnReference $ Expr.unqualified colname) compareVal)
    fieldAlias
    Marshall.double

-- | Create a named HNSW index for vector similarity search
mkNamedVectorHnswIndexDefinition ::
  -- | The name of the index to be created
  String ->
  -- | The field name to create the index over
  Marshall.FieldName ->
  -- | The distance operator to use for the index
  String ->
  -- | Optional index parameters (m, ef_construction)
  Maybe (Int, Int) ->
  -- | The index definition
  IndexDefinition.IndexDefinition
mkNamedVectorHnswIndexDefinition name fieldName distanceOp mbParams =
  IndexDefinition.mkNamedIndexDefinition
    Expr.NonUniqueIndex
    name
    (RawSql.unsafeFromRawSql $ vectorHnswIndexFieldsExpr fieldName distanceOp mbParams)

-- | Create a named IVFFlat index for vector similarity search
mkNamedVectorIvfflatIndexDefinition ::
  -- | The name of the index to be created
  String ->
  -- | The field name to create the index over
  Marshall.FieldName ->
  -- | The distance operator to use for the index
  String ->
  -- | Optional index parameters (lists)
  Maybe Int ->
  -- | The index definition
  IndexDefinition.IndexDefinition
mkNamedVectorIvfflatIndexDefinition name fieldName distanceOp mbLists =
  IndexDefinition.mkNamedIndexDefinition
    Expr.NonUniqueIndex
    name
    (RawSql.unsafeFromRawSql $ vectorIvfflatIndexFieldsExpr fieldName distanceOp mbLists)

-- | Generate HNSW index fields expression
vectorHnswIndexFieldsExpr ::
  Marshall.FieldName ->
  String ->
  Maybe (Int, Int) ->
  RawSql.RawSql
vectorHnswIndexFieldsExpr fieldName distanceOp mbParams =
  let
    fieldExpr = RawSql.toRawSql (Marshall.fieldNameToColumnName fieldName)
    opExpr = RawSql.fromString distanceOp
    paramsExpr = case mbParams of
      Nothing -> mempty
      Just (m, ef) ->
        RawSql.parenthesized $
          RawSql.fromString "m = "
            <> RawSql.intDecLiteral m
            <> RawSql.commaSpace
            <> RawSql.fromString "ef_construction = "
            <> RawSql.intDecLiteral ef
  in
    RawSql.fromString "USING hnsw "
      <> RawSql.parenthesized (fieldExpr <> RawSql.space <> opExpr)
      <> paramsExpr

-- | Generate IVFFlat index fields expression
vectorIvfflatIndexFieldsExpr ::
  Marshall.FieldName ->
  String ->
  Maybe Int ->
  RawSql.RawSql
vectorIvfflatIndexFieldsExpr fieldName distanceOp mbLists =
  let
    fieldExpr = RawSql.toRawSql (Marshall.fieldNameToColumnName fieldName)
    opExpr = RawSql.fromString distanceOp
    listsExpr = case mbLists of
      Nothing -> mempty
      Just lists ->
        RawSql.parenthesized $
          RawSql.fromString "lists = " <> RawSql.intDecLiteral lists
  in
    RawSql.fromString "USING ivfflat "
      <> RawSql.parenthesized (fieldExpr <> RawSql.space <> opExpr)
      <> listsExpr
