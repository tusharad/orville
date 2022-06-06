{-# LANGUAGE GADTs #-}

module Orville.PostgreSQL.Internal.Update
  ( Update,
    updateToUpdateExpr,
    executeUpdate,
    executeUpdateReturnEntities,
    updateToTableReturning,
    updateToTable,
    updateToTableFieldsReturning,
    updateToTableFields,
  )
where

import qualified Orville.PostgreSQL.Internal.Execute as Execute
import qualified Orville.PostgreSQL.Internal.Expr as Expr
import qualified Orville.PostgreSQL.Internal.MonadOrville as MonadOrville
import Orville.PostgreSQL.Internal.PrimaryKey (primaryKeyEqualsExpr)
import Orville.PostgreSQL.Internal.ReturningOption (NoReturningClause, ReturningClause, ReturningOption (WithReturning, WithoutReturning))
import qualified Orville.PostgreSQL.Internal.SelectOptions as SelectOptions
import Orville.PostgreSQL.Internal.SqlMarshaller (AnnotatedSqlMarshaller, marshallEntityToSetClauses, unannotatedSqlMarshaller)
import Orville.PostgreSQL.Internal.TableDefinition (HasKey, TableDefinition, mkTableReturningClause, tableMarshaller, tableName, tablePrimaryKey)

{- | Represents an @UPDATE@ statement that can be executed against a database. An 'Update' has a
  'SqlMarshaller' bound to it that, when the update returns data from the database, will be used to
  decode the database result set when it is executed.
-}
data Update readEntity returningClause where
  UpdateNoReturning :: Expr.UpdateExpr -> Update readEntity NoReturningClause
  UpdateReturning :: AnnotatedSqlMarshaller writeEntity readEntity -> Expr.UpdateExpr -> Update readEntity ReturningClause

{- |
  Extracts the query that will be run when the update is executed. Normally you
  don't want to extract the query and run it yourself, but this function is
  useful to view the query for debugging or query explanation.
-}
updateToUpdateExpr :: Update readEntity returningClause -> Expr.UpdateExpr
updateToUpdateExpr (UpdateNoReturning expr) = expr
updateToUpdateExpr (UpdateReturning _ expr) = expr

{- |
  Executes the database query for the 'Update' and returns '()'.
-}
executeUpdate :: MonadOrville.MonadOrville m => Update readEntity returningClause -> m ()
executeUpdate =
  Execute.executeVoid . updateToUpdateExpr

{- |
  Executes the database query for the 'Update' and uses its
  'AnnotatedSqlMarshaller' to decode any rows that were just updated, as
  returned via a RETURNING clause.
-}
executeUpdateReturnEntities :: MonadOrville.MonadOrville m => Update readEntity ReturningClause -> m [readEntity]
executeUpdateReturnEntities (UpdateReturning marshaller expr) =
  Execute.executeAndDecode expr marshaller

{- |
  Builds an 'Update' that will update all of the writable columns described in the
  'TableDefinition' without returning the data as seen by the database.
-}
updateToTable ::
  TableDefinition (HasKey key) writeEntity readEntity ->
  key ->
  writeEntity ->
  Update readEntity NoReturningClause
updateToTable =
  updateTable WithoutReturning

{- |
  Builds an 'Update' that will update all of the writable columns described in the
  'TableDefinition' and returning the data as seen by the database. This is useful for getting
  database managed columns such as auto-incrementing identifiers and sequences.
-}
updateToTableReturning ::
  TableDefinition (HasKey key) writeEntity readEntity ->
  key ->
  writeEntity ->
  Update readEntity ReturningClause
updateToTableReturning =
  updateTable WithReturning

-- an internal helper function for creating an update with a given
-- `ReturningOption` to a single entity in a table, setting all the
-- columns found in the table's SQL marshaller.
updateTable ::
  ReturningOption returningClause ->
  TableDefinition (HasKey key) writeEntity readEntity ->
  key ->
  writeEntity ->
  Update readEntity returningClause
updateTable returningOption tableDef key writeEntity =
  let setClauses =
        marshallEntityToSetClauses
          (unannotatedSqlMarshaller $ tableMarshaller tableDef)
          writeEntity

      isEntityKey =
        SelectOptions.whereBooleanExpr $
          primaryKeyEqualsExpr
            (tablePrimaryKey tableDef)
            key
   in updateFields
        returningOption
        tableDef
        setClauses
        (Just isEntityKey)

updateToTableFields ::
  TableDefinition key writeEntity readEntity ->
  [Expr.SetClause] ->
  Maybe SelectOptions.WhereCondition ->
  Update readEntity NoReturningClause
updateToTableFields =
  updateFields WithoutReturning

updateToTableFieldsReturning ::
  TableDefinition key writeEntity readEntity ->
  [Expr.SetClause] ->
  Maybe SelectOptions.WhereCondition ->
  Update readEntity ReturningClause
updateToTableFieldsReturning =
  updateFields WithReturning

updateFields ::
  ReturningOption returningClause ->
  TableDefinition key writeEntity readEntity ->
  [Expr.SetClause] ->
  Maybe SelectOptions.WhereCondition ->
  Update readEntity returningClause
updateFields returingOption tableDef setClauses mbWhereCondition =
  let whereClause =
        fmap
          (Expr.whereClause . SelectOptions.whereConditionToBooleanExpr)
          mbWhereCondition
   in rawUpdateExpr returingOption (tableMarshaller tableDef) $
        Expr.updateExpr
          (tableName tableDef)
          (Expr.setClauseList setClauses)
          whereClause
          (mkTableReturningClause returingOption tableDef)

{- |
  Builds an 'Update' that will execute the specified query and use the given 'AnnotatedSqlMarshaller' to
  decode it. It is up to the caller to ensure that the given 'Expr.UpdateExpr' makes sense and
  produces a value that can be stored, as well as returning a result that the 'AnnotatedSqlMarshaller' can
  decode.

  This is the lowest level of escape hatch available for 'Update'. The caller can build any query
  that Orville supports using the expression building functions, or use @RawSql.fromRawSql@ to build
  a raw 'Expr.UpdateExpr'.
-}
rawUpdateExpr :: ReturningOption returningClause -> AnnotatedSqlMarshaller writeEntity readEntity -> Expr.UpdateExpr -> Update readEntity returningClause
rawUpdateExpr WithReturning marshaller = UpdateReturning marshaller
rawUpdateExpr WithoutReturning _ = UpdateNoReturning
