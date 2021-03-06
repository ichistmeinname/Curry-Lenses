module CommentView (
 wComment, wCommentType, blankCommentView,
 createCommentView, editCommentView, showCommentView, listCommentView
 ) where

import HTML
import Time
import Sort
import Spicey
import Blog
import BlogEntitiesToHtml

import BlogLenses

--- The WUI specification for the entity type Comment.
--- It also includes fields for associated entities.
wComment :: [Entry] -> WuiLensSpec (String,String,CalendarTime,Entry)
wComment entryList =
  withRendering
   (w4Tuple wRequiredString wRequiredString wDateType
     (wSelect entryToShortView entryList))
   (renderLabels commentLabelList)

wCommentEdit :: [Entry] -> WuiLensSpec (String,Entry)
wCommentEdit entryList =
  withRendering
   (wPair wRequiredString
     (wSelect entryToShortView entryList))
   (renderLabels commentEditLabelList)

wCommentType :: Entry -> [Entry] -> WuiLensSpec Comment
wCommentType entry entryList =
  transformWSpec (commentWOKey entry) $ wComment entryList

wCommentEditType :: Entry -> [Entry] -> WuiLensSpec Comment
wCommentEditType entry entryList =
  transformWSpec (commentZoom entry) $ wCommentEdit entryList

--- Supplies a WUI form to create a new Comment entity.
--- The fields of the entity have some default values.
blankCommentView
 :: CalendarTime -> [Entry]
  -> (Bool -> (String,String,CalendarTime,Entry) -> Controller) -> [HtmlExp]
blankCommentView ctime possibleEntrys controller =
  createCommentView [] [] ctime (head possibleEntrys) possibleEntrys
   controller

--- Supplies a WUI form to create a new Comment entity.
--- Takes default values to be prefilled in the form fields.
createCommentView
 :: String -> String -> CalendarTime -> Entry -> [Entry]
  -> (Bool -> (String,String,CalendarTime,Entry) -> Controller) -> [HtmlExp]
createCommentView defaultText defaultAuthor defaultDate defaultEntry
                  possibleEntrys controller =
  let initdata = (defaultText,defaultAuthor,defaultDate,defaultEntry)
      wuiframe = wuiEditForm "Create new Comment" "create"
                  (controller False initdata)
      (hexp ,handler) = wuiWithErrorForm (wComment possibleEntrys) initdata
                         (nextControllerForData (controller True))
                         (wuiFrameToForm wuiframe)
   in wuiframe hexp handler

--- Supplies a WUI form to edit the given Comment entity.
--- Takes also associated entities and a list of possible associations
--- for every associated entity type.
editCommentView
 :: Comment -> Entry -> [Entry] -> (Bool -> Comment -> Controller)
  -> [HtmlExp]
editCommentView comment relatedEntry possibleEntrys controller =
  let initdata = comment
      wuiframe = wuiEditForm "Edit Comment" "change"
                  (controller False initdata)
      (hexp ,handler) = wuiWithErrorForm
                         (wCommentEditType relatedEntry possibleEntrys)
                         initdata (nextControllerForData (controller True))
                         (wuiFrameToForm wuiframe)
   in wuiframe hexp handler

--- Supplies a view to show the details of a Comment.
showCommentView :: Comment -> Entry -> Controller -> [HtmlExp]
showCommentView comment relatedEntry controller =
  commentToDetailsView comment relatedEntry ++
   [spButton "back to Comment list" (nextController controller)]

--- Compares two Comment entities. This order is used in the list view.
leqComment :: Comment -> Comment -> Bool
leqComment x1 x2 =
  (commentText x1,commentAuthor x1,commentDate x1) <=
   (commentText x2,commentAuthor x2,commentDate x2)

--- Supplies a list view for a given list of Comment entities.
--- Shows also buttons to show, delete, or edit entries.
--- The arguments are the list of Comment entities
--- and the controller functions to show, delete and edit entities.
listCommentView
 :: [Comment] -> (Comment -> Controller) -> (Comment -> Controller)
  -> (Comment -> Bool -> Controller) -> [HtmlExp]
listCommentView comments showCommentController editCommentController
                deleteCommentController =
  [h1 [htxt "Comment list"]
  ,spTable
    ([take 3 commentLabelList] ++
     map listComment (mergeSort leqComment comments))]
  where listComment :: Comment -> [[HtmlExp]]
        listComment comment =
          commentToListView comment ++
           [[spSmallButton "show"
              (nextController (showCommentController comment))
            ,spSmallButton "edit"
              (nextController (editCommentController comment))
            ,spSmallButton "delete"
              (confirmNextController
                (h3
                  [htxt
                    (concat
                      ["Really delete entity \"",commentToShortView comment
                      ,"\"?"])])
                (deleteCommentController comment))]]
