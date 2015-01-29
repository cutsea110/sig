module Ext.Projectable (like) where

import Data.String (IsString)
import Language.SQL.Keyword (Keyword (LIKE))
import qualified Language.SQL.Keyword as SQL
import Database.Relational.Query

like :: (SqlProjectable p, ProjectableShowSql p, IsString a)
        => p a -> p b -> p (Maybe Bool)
like = unsafeBinOp (SQL.defineBinOp LIKE)
