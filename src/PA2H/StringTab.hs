module StringTab(ObjectId(),TypeId(),IntId(),
                  ObjectTable(),IntTable(),TypeTable(),
                  Tables,initTables,
                  addInt,addType,addObject,
                  getInt,getType,getObject) where 

import qualified Data.Map as Map
import qualified Data.IntMap as IntMap

  --Create some tables to store strings representing integers, objects and types
  -- Hold them behind the scenes as Data.Map also have a Int of the next availible id.
  -- Also have an IntMap for reverse lookup.

data StringTable = StringTable (Map.Map String Int) (IntMap.IntMap String) Int

newtype IntId = IntId Int deriving (Eq,Show)
newtype TypeId = TypeId Int deriving (Eq,Show)
newtype ObjectId = ObjectId Int deriving (Eq,Show)

newtype IntTable = IntTable StringTable
newtype TypeTable = TypeTable StringTable
newtype ObjectTable = ObjectTable StringTable

type Tables = (IntTable,TypeTable,ObjectTable)

addString :: StringTable -> String -> (Int,StringTable)
addString (StringTable sMap iMap i) s = maybe newstr foundstr look 
  where look = Map.lookup s sMap
        newstr = (i,StringTable insertstr inserti nexti)
        nexti  = i + 1
        insertstr = Map.insert s i sMap
        inserti = IntMap.insert i s iMap
        foundstr val = (val,StringTable sMap iMap i)
  
initStringTable :: StringTable
initStringTable = StringTable Map.empty IntMap.empty 0

initTables :: Tables
initTables = (IntTable initStringTable
              ,TypeTable initStringTable
              ,ObjectTable initStringTable)

addInt :: Tables -> String -> (IntId,Tables)
addInt (IntTable itable,tt,ot) s = (iid,tables)
  where addStr = addString itable s
        tables = (IntTable (snd addStr),tt,ot)
        iid = IntId (fst addStr)

addType :: Tables -> String -> (TypeId,Tables)
addType (it,TypeTable ttable,ot) s = (tid,tables)
  where addStr = addString ttable s
        tables = (it,TypeTable (snd addStr),ot)
        tid = TypeId (fst addStr)

addObject :: Tables -> String -> (ObjectId,Tables)
addObject (it,tt,ObjectTable otable) s = (oid,tables)
  where addStr = addString otable s
        tables = (it,tt,ObjectTable (snd addStr))
        oid = ObjectId (fst addStr)

--Stop the maybe leaking to the main loop of the program
-- non ideal but gives a nicer interface.
getString :: StringTable -> Int -> String
getString (StringTable _ iMap _) i = maybe "ERROR table leak" id val
  where val = IntMap.lookup i iMap

getInt :: Tables -> IntId -> String
getInt ( (IntTable it) ,_,_) (IntId i) = getString it i

getType :: Tables -> TypeId -> String
getType (_, (TypeTable tt) ,_) (TypeId i) = getString tt i

getObject :: Tables -> ObjectId -> String
getObject (_,_, (ObjectTable ot)) (ObjectId i) = getString ot i

