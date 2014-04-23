module TagView (
 wTag, {-tuple2Tag, tag2Tuple,-} wTagType, blankTagView, createTagView,
 editTagView, showTagView, listTagView
 ) where

import HTML
import Time
import Sort
import Spicey
import Blog
import BlogEntitiesToHtml

import Monadic

--- The WUI specification for the entity type Tag.
wTag :: WuiLensSpec String
wTag = withRendering wRequiredString (renderLabels tagLabelList)

-- --- Transformation from data of a WUI form to entity type Tag.
-- tuple2Tag :: Tag -> String -> Tag
-- tuple2Tag tagToUpdate name = setTagName tagToUpdate name

-- --- Transformation from entity type Tag to a tuple
-- --- which can be used in WUI specifications.
-- tag2Tuple :: Tag -> String
-- tag2Tuple tag = tagName tag

--- WUI Type for editing or creating Tag entities.
--- Includes fields for associated entities.
wTagType :: Tag -> WuiLensSpec Tag
wTagType tag = transformWSpec (isoLens inn out <.> keepFst) wTag
 where
  inn (key,name)     = Tag key name
  out (Tag key name) = (key,name)

--- Supplies a WUI form to create a new Tag entity.
--- The fields of the entity have some default values.
blankTagView :: (Bool -> String -> Controller) -> [HtmlExp]
blankTagView controller = createTagView [] controller

--- Supplies a WUI form to create a new Tag entity.
--- Takes default values to be prefilled in the form fields.
createTagView :: String -> (Bool -> String -> Controller) -> [HtmlExp]
createTagView defaultName controller =
  let initdata = defaultName
      
      wuiframe = wuiEditForm "Create new Tag" "create"
                  (controller False initdata)
      
      (hexp ,handler) = wuiWithErrorForm wTag initdata
                         (nextControllerForData (controller True))
                         (wuiFrameToForm wuiframe)
   in wuiframe hexp handler

--- Supplies a WUI form to edit the given Tag entity.
--- Takes also associated entities and a list of possible associations
--- for every associated entity type.
editTagView :: Tag -> (Bool -> Tag -> Controller) -> [HtmlExp]
editTagView tag controller =
  let initdata = tag
      
      wuiframe = wuiEditForm "Edit Tag" "change" (controller False initdata)
      
      (hexp ,handler) = wuiWithErrorForm (wTagType tag) initdata
                         (nextControllerForData (controller True))
                         (wuiFrameToForm wuiframe)
   in wuiframe hexp handler

--- Supplies a view to show the details of a Tag.
showTagView :: Tag -> Controller -> [HtmlExp]
showTagView tag controller =
  tagToDetailsView tag ++
   [spButton "back to Tag list" (nextController controller)]

--- Compares two Tag entities. This order is used in the list view.
leqTag :: Tag -> Tag -> Bool
leqTag x1 x2 = tagName x1 <= tagName x2

--- Supplies a list view for a given list of Tag entities.
--- Shows also buttons to show, delete, or edit entries.
--- The arguments are the list of Tag entities
--- and the controller functions to show, delete and edit entities.
listTagView
 :: [Tag] -> (Tag -> Controller) -> (Tag -> Controller)
  -> (Tag -> Bool -> Controller) -> [HtmlExp]
listTagView tags showTagController editTagController deleteTagController =
  [h1 [htxt "Tag list"]
  ,spTable ([take 1 tagLabelList] ++ map listTag (mergeSort leqTag tags))]
  where listTag :: Tag -> [[HtmlExp]]
        listTag tag =
          tagToListView tag ++
           [[spSmallButton "show" (nextController (showTagController tag))
            ,spSmallButton "edit" (nextController (editTagController tag))
            ,spSmallButton "delete"
              (confirmNextController
                (h3
                  [htxt
                    (concat
                      ["Really delete entity \"",tagToShortView tag,"\"?"])])
                (deleteTagController tag))]]
