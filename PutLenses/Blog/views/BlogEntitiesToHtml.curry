module BlogEntitiesToHtml where

import WUI
import HTML
import Time
import Spicey
import Blog

--- The list view of a Entry entity in HTML format.
--- This view is used in a row of a table of all entities.
entryToListView :: Entry -> [[HtmlExp]]
entryToListView entry =
  [[stringToHtml (entryTitle entry)],[stringToHtml (entryText entry)]
  ,[stringToHtml (entryAuthor entry)],[calendarTimeToHtml (entryDate entry)]]

--- The short view of a Entry entity as a string.
--- This view is used in menus and comments to refer to a Entry entity.
entryToShortView :: Entry -> String
entryToShortView entry = entryTitle entry

--- The detailed view of a Entry entity in HTML format.
--- It also takes associated entities for every associated entity type.
entryToDetailsView :: Entry -> [Tag] -> [HtmlExp]
entryToDetailsView entry tags =
  [spTable
    (map (\ (label ,value) -> [label,value])
      (zip entryLabelList detailedView))]
  where detailedView = [[stringToHtml (entryTitle entry)]
                       ,[stringToHtml (entryText entry)]
                       ,[stringToHtml (entryAuthor entry)]
                       ,[calendarTimeToHtml (entryDate entry)]
                       ,[htxt (unwords (map tagToShortView tags))]]

--- The labels of a Entry entity, as used in HTML tables.
entryLabelList :: [[HtmlExp]]
entryLabelList =
  [[textstyle "label label_for_type_string" "Title"]
  ,[textstyle "label label_for_type_string" "Text"]
  ,[textstyle "label label_for_type_string" "Author"]
  ,[textstyle "label label_for_type_calendarTime" "Date"]
  ,[textstyle "label label_for_type_relation" "Tag"]]

entryEditLabelList :: [[HtmlExp]]
entryEditLabelList =
  [[textstyle "label label_for_type_string" "Title"]
  ,[textstyle "label label_for_type_string" "Text"]
  ,[textstyle "label label_for_type_relation" "Tag"]]

--- The list view of a Comment entity in HTML format.
--- This view is used in a row of a table of all entities.
commentToListView :: Comment -> [[HtmlExp]]
commentToListView comment =
  [[stringToHtml (commentText comment)],[stringToHtml (commentAuthor comment)]
  ,[calendarTimeToHtml (commentDate comment)]]

--- The short view of a Comment entity as a string.
--- This view is used in menus and comments to refer to a Comment entity.
commentToShortView :: Comment -> String
commentToShortView comment = commentText comment

--- The detailed view of a Comment entity in HTML format.
--- It also takes associated entities for every associated entity type.
commentToDetailsView :: Comment -> Entry -> [HtmlExp]
commentToDetailsView comment relatedEntry =
  [spTable
    (map (\ (label ,value) -> [label,value])
      (zip commentLabelList detailedView))]
  where detailedView = [[stringToHtml (commentText comment)]
                       ,[stringToHtml (commentAuthor comment)]
                       ,[calendarTimeToHtml (commentDate comment)]
                       ,[htxt (entryToShortView relatedEntry)]]

--- The labels of a Comment entity, as used in HTML tables.
commentLabelList :: [[HtmlExp]]
commentLabelList =
  [[textstyle "label label_for_type_string" "Text"]
  ,[textstyle "label label_for_type_string" "Author"]
  ,[textstyle "label label_for_type_calendarTime" "Date"]
  ,[textstyle "label label_for_type_relation" "Entry"]]

commentEditLabelList :: [[HtmlExp]]
commentEditLabelList =
  [[textstyle "label label_for_type_string" "Text"]
  ,[textstyle "label label_for_type_relation" "Entry"]]


--- The list view of a Tag entity in HTML format.
--- This view is used in a row of a table of all entities.
tagToListView :: Tag -> [[HtmlExp]]
tagToListView tag = [[stringToHtml (tagName tag)]]

--- The short view of a Tag entity as a string.
--- This view is used in menus and comments to refer to a Tag entity.
tagToShortView :: Tag -> String
tagToShortView tag = tagName tag

--- The detailed view of a Tag entity in HTML format.
tagToDetailsView :: Tag -> [HtmlExp]
tagToDetailsView tag =
  [spTable
    (map (\ (label ,value) -> [label,value]) (zip tagLabelList detailedView))]
  where detailedView = [[stringToHtml (tagName tag)]]

--- The labels of a Tag entity, as used in HTML tables.
tagLabelList :: [[HtmlExp]]
tagLabelList = [[textstyle "label label_for_type_string" "Name"]]
