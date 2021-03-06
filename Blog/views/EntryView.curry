module EntryView (
 wEntry, wEntryType, blankEntryView,
 createEntryView, editEntryView, showEntryView, listEntryView
 ) where

import HTML
import Time
import Sort
import Spicey
import Blog
import BlogEntitiesToHtml

import BlogLenses

--------------------------------------------------------------------------------
--------------------------------------------------------------------------------
--------------------------------------------------------------------------------

--- The WUI specification for the entity type Entry.
--- It also includes fields for associated entities.
wEntry :: [Tag] -> WuiLensSpec (String,String,String,CalendarTime,[Tag])
wEntry tagList =
  withRendering
   (w5Tuple wRequiredString wRequiredString wRequiredString wDateType
     (wMultiCheckSelect (\ tag -> [htxt (tagToShortView tag)]) tagList))
   (renderLabels entryLabelList)

wEntryEdit :: [Tag] -> WuiLensSpec (String,String,[Tag])
wEntryEdit tagList =
  withRendering
   (wTriple wRequiredString wRequiredString
     (wMultiCheckSelect (\ tag -> [htxt (tagToShortView tag)]) tagList))
   (renderLabels entryEditLabelList)

--- WUI Type for editing or creating Entry entities.
--- Includes fields for associated entities.
wEntryType :: Entry -> [Tag] -> WuiLensSpec (Entry,[Tag])
wEntryType entry tagList =
  transformWSpec entryWOKey (wEntry tagList)

wEntryEditType :: Entry -> [Tag] -> WuiLensSpec (Entry,[Tag])
wEntryEditType entry tagList =
  transformWSpec entryZoom (wEntryEdit tagList)

--- Supplies a WUI form to create a new Entry entity.
--- The fields of the entity have some default values.
blankEntryView
 :: CalendarTime -> [Tag]
  -> (Bool -> (String,String,String,CalendarTime,[Tag]) -> Controller)
  -> [HtmlExp]
blankEntryView ctime possibleTags controller =
  createEntryView "" "" "" ctime [] possibleTags controller

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
      
      wuiframe = wuiEditForm "Create new Entry" "create"
                  (controller False initdata)
      
      (hexp,handler) = wuiWithErrorForm (wEntry possibleTags) initdata
                         (nextControllerForData (controller True))
                         (wuiFrameToForm wuiframe)
   in wuiframe hexp handler

--- Supplies a WUI form to edit the given Entry entity.
--- Takes also associated entities and a list of possible associations
--- for every associated entity type.
editEntryView
 :: (Entry,[Tag]) -> [Tag] -> (Bool -> (Entry,[Tag]) -> Controller)
  -> [HtmlExp]
editEntryView (entry ,tags) possibleTags controller =
  let initdata = (entry,tags)
      
      wuiframe = wuiEditForm "Edit Entry" "change" (controller False initdata)
      
      (hexp ,handler) = wuiWithErrorForm (wEntryEditType entry possibleTags)
                         initdata (nextControllerForData (controller True))
                         (wuiFrameToForm wuiframe)
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
