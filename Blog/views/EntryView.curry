module EntryView (
 wEntry, tuple2Entry, entry2Tuple, wEntryType, blankEntryView,
 createEntryView, editEntryView, showEntryView, listEntryView
 ) where

import WUILenses
import HTML
import Time
import Sort
import Spicey
import Blog
import BlogEntitiesToHtml

import Monadic

--------------------------------------------------------------------------------
------------------------------------ helper ------------------------------------
--------------------------------------------------------------------------------

wDateType' :: WuiLensSpec CalendarTime
wDateType' = transformWSpec dateLens wDate'
 where
  dateLens = isoLens inn out
  inn :: (Int, Int, Int) -> CalendarTime
  inn (day, month, year) = CalendarTime year month day 0 0 0 0

  out :: CalendarTime -> (Int, Int, Int)
  out (CalendarTime year month day _ _ _ _) = (day, month, year)

--- A WUI for manipulating date entities.
wDate' :: WuiLensSpec (Int, Int, Int)
wDate' = wTriple (wSelectInt [1..31])
                 (wSelectInt [1..12])
                 (wSelectInt [1950..2050])

-- A standard HTML frame for editing data with WUIs.
wuiEditForm' :: String -> String -> Controller
           -> HtmlExp -> WuiHandler -> [HtmlExp]
wuiEditForm' title buttontag controller hexp handler =
  [h1 [htxt title],
   blockstyle "editform" [hexp],
   wuiHandler2button buttontag handler `addClass` "btn btn-primary",
   spButton "cancel" (nextController (cancelOperation >> controller))]

--- Transforms a WUI frame into a standard form.
wuiFrameToForm' :: (HtmlExp -> WuiHandler -> [HtmlExp])
               -> HtmlExp -> WuiHandler -> IO HtmlForm
wuiFrameToForm' wframe hexp wuihandler = getForm (wframe hexp wuihandler)

--------------------------------------------------------------------------------
--------------------------------------------------------------------------------
--------------------------------------------------------------------------------

--- The WUI specification for the entity type Entry.
--- It also includes fields for associated entities.
wEntry :: [Tag] -> WuiLensSpec (String,String,String,CalendarTime,[Tag])
wEntry tagList =
  withRendering
   (w5Tuple wRequiredString wRequiredString wRequiredString wDateType'
     (wMultiCheckSelect (\ tag -> [htxt (tagToShortView tag)]) tagList))
   (renderLabels entryLabelList)

wEntryEdit :: [Tag] -> WuiLensSpec (String,String,[Tag])
wEntryEdit tagList =
  withRendering
   (wTriple wRequiredString wRequiredString
     (wMultiCheckSelect (\ tag -> [htxt (tagToShortView tag)]) tagList))
   (renderLabels entryEditLabelList)

--- Transformation from data of a WUI form to entity type Entry.
tuple2Entry
 :: Entry -> (String,String,String,CalendarTime,[Tag]) -> (Entry,[Tag])
tuple2Entry entryToUpdate (title ,text ,author ,date ,tags) =
  (setEntryTitle
    (setEntryText (setEntryAuthor (setEntryDate entryToUpdate date) author)
      text)
    title,tags)

--- Transformation from entity type Entry to a tuple
--- which can be used in WUI specifications.
entry2Tuple :: (Entry,[Tag]) -> (String, String, String, CalendarTime, [Tag])
entry2Tuple (entry, tags) =
  (entryTitle entry,entryText entry,entryAuthor entry,entryDate entry,tags)

--- WUI Type for editing or creating Entry entities.
--- Includes fields for associated entities.
wEntryType :: Entry -> [Tag] -> WuiLensSpec (Entry,[Tag])
wEntryType entry tagList =
  transformWSpec entryWOKey (wEntry tagList)
 where
  entryWOKey :: Lens (Entry,[Tag]) (String,String,String,CalendarTime,[Tag])
  entryWOKey = isoLens inn out <.> keepFst
  inn (k, (t1,t2,a,d,tags)) = (Entry k t1 t2 a d, tags)
  out (Entry k t1 t2 a d, tags) = (k, (t1,t2,a,d,tags))
  -- transformWSpec (tuple2Entry entry, entry2Tuple) (wEntry tagList)

wEntryEditType :: Entry -> [Tag] -> WuiLensSpec (Entry,[Tag])
wEntryEditType entry tagList =
  transformWSpec entryZoom (wEntryEdit tagList)
 where
  entryZoom :: Lens (Entry,[Tag]) (String, String, [Tag])
  entryZoom = isoLens inn out <.> keepFst
  inn ((k,a,d),(t1,t2,tags))  = (Entry k t1 t2 a d, tags)
  out (Entry k t1 t2 a d, tags) = ((k,a,d),(t1,t2,tags))
 -- transformWSpec (toEntry entry, toTuple) (wEntryEdit tagList)
 -- where
 --  toEntry (Entry k _ _ a d) (t1,t2,tags) = (Entry k t1 t2 a d,tags)
 --  toTuple (Entry _ t1 t2 _ _, tags)      = (t1,t2,tags)

--- Supplies a WUI form to create a new Entry entity.
--- The fields of the entity have some default values.
blankEntryView
 :: CalendarTime -> [Tag]
  -> (Bool -> (String,String,String,CalendarTime,[Tag]) -> Controller)
  -> [HtmlExp]
blankEntryView ctime possibleTags controller =
  createEntryView [] [] "Sandra" ctime [] possibleTags controller

--- Supplies a WUI form to create a new Entry entity.
--- Takes default values to be prefilled in the form fields.
createEntryView
 :: String -> String -> String -> CalendarTime -> [Tag] -> [Tag]
  -> (Bool -> (String,String,String,CalendarTime,[Tag]) -> Controller)
  -> [HtmlExp]
createEntryView defaultTitle defaultText defaultAuthor defaultDate defaultTags
                possibleTags controller =
  let initdata = (defaultTitle,defaultText,defaultAuthor,defaultDate
                 ,defaultTags)
      
      wuiframe = wuiEditForm' "Create new Entry" "create"
                  (controller False initdata)
      
      (hexp,handler) = wuiWithErrorForm (wEntry possibleTags) initdata
                         (nextControllerForData (controller True))
                         (wuiFrameToForm' wuiframe)
   in wuiframe hexp handler

--- Supplies a WUI form to edit the given Entry entity.
--- Takes also associated entities and a list of possible associations
--- for every associated entity type.
editEntryView
 :: (Entry,[Tag]) -> [Tag] -> (Bool -> (Entry,[Tag]) -> Controller)
  -> [HtmlExp]
editEntryView (entry ,tags) possibleTags controller =
  let initdata = (entry,tags)
      
      wuiframe = wuiEditForm' "Edit Entry" "change" (controller False initdata)
      
      (hexp ,handler) = wuiWithErrorForm (wEntryEditType entry possibleTags)
                         initdata (nextControllerForData (controller True))
                         (wuiFrameToForm' wuiframe)
   in wuiframe hexp handler

--- Supplies a view to show the details of a Entry.
showEntryView :: Entry -> [Tag] -> Controller -> [HtmlExp]
showEntryView entry tags controller =
  entryToDetailsView entry tags ++
   [spButton "back to Entry list" (nextController controller)]

--- Compares two Entry entities. This order is used in the list view.
leqEntry :: Entry -> Entry -> Bool
leqEntry x1 x2 =
  (entryTitle x1,entryText x1,entryAuthor x1,entryDate x1) <=
   (entryTitle x2,entryText x2,entryAuthor x2,entryDate x2)

--- Supplies a list view for a given list of Entry entities.
--- Shows also buttons to show, delete, or edit entries.
--- The arguments are the list of Entry entities
--- and the controller functions to show, delete and edit entities.
listEntryView
 :: [Entry] -> (Entry -> Controller) -> (Entry -> Controller)
  -> (Entry -> Bool -> Controller) -> [HtmlExp]
listEntryView entrys showEntryController editEntryController
              deleteEntryController =
  [h1 [htxt "Entry list"]
  ,spTable
    ([take 4 entryLabelList] ++ map listEntry (mergeSort leqEntry entrys))]
  where listEntry :: Entry -> [[HtmlExp]]
        listEntry entry =
          entryToListView entry ++
           [[spSmallButton "show" (nextController (showEntryController entry))
            ,spSmallButton "edit" (nextController (editEntryController entry))
            ,spSmallButton "delete"
              (confirmNextController
                (h3
                  [htxt
                    (concat
                      ["Really delete entity \"",entryToShortView entry
                      ,"\"?"])])
                (deleteEntryController entry))]]
