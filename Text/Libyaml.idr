module Text.Libyaml

import CFFI

%include C "vendor/libyaml/include/yaml.h"
%link C "vendor/libyaml/src/.libs/libyaml.so"

%include C "helper.h"
%link C "helper.o"

%access private

-- TODO: figure out whether there's need for managed pointers?

data Parser = MkParser Ptr

parserSize : Int
parserSize = 480

data EventRaw = MkEventRaw Ptr
eventSize : Int
eventSize = 104

parser_initialize : IO Parser
parser_initialize = do
  ptr <- malloc parserSize
  foreign FFI_C "yaml_parser_initialize" (Ptr -> IO Int) ptr
  -- TODO: handle res
  pure (MkParser ptr)

parser_delete : Parser -> IO ()
parser_delete (MkParser ptr) = do
  foreign FFI_C "yaml_parser_delete" (Ptr -> IO Int) ptr
  -- TODO: do something with return value?
  free ptr

set_input_string : Parser -> String -> IO ()
set_input_string (MkParser ptr) s = do
  let len = toIntNat (length s)
  foreign FFI_C "yaml_parser_set_input_string"
    (Ptr -> String -> Int -> IO ()) ptr s len

parser_parse : Parser -> IO EventRaw
parser_parse (MkParser p) = do
  e <- malloc eventSize
  -- TODO: handle res
  foreign FFI_C "yaml_parser_parse" (Ptr -> Ptr -> IO ()) p e
  pure (MkEventRaw e)

public export
AnchorName : Type
AnchorName = String

public export
Anchor : Type
Anchor = Maybe AnchorName

public export
data Style = Any
           | Plain
           | SingleQuoted
           | DoubleQuoted
           | Literal
           | Folded
           | PlainNoTag

intToStyle : Int -> Style
intToStyle 0 = Any
intToStyle 1 = Any
intToStyle 2 = Any
intToStyle 3 = Any
intToStyle 4 = Any
intToStyle 5 = Any
intToStyle 6 = Any

public export
data Tag = StrTag
         | FloatTag
         | NullTag
         | BoolTag
         | SetTag
         | IntTag
         | SeqTag
         | MapTag
         | UriTag String
         | NoTag

tagToString : Tag -> String
tagToString StrTag = "tag:yaml.org,2002:str"
tagToString FloatTag = "tag:yaml.org,2002:float"
tagToString NullTag = "tag:yaml.org,2002:null"
tagToString BoolTag = "tag:yaml.org,2002:bool"
tagToString SetTag = "tag:yaml.org,2002:set"
tagToString IntTag = "tag:yaml.org,2002:int"
tagToString SeqTag = "tag:yaml.org,2002:seq"
tagToString MapTag = "tag:yaml.org,2002:map"
tagToString (UriTag s) = s
tagToString NoTag = ""

stringToTag : String -> Tag
stringToTag "tag:yaml.org,2002:str" = StrTag
stringToTag "tag:yaml.org,2002:float" = FloatTag
stringToTag "tag:yaml.org,2002:null" = NullTag
stringToTag "tag:yaml.org,2002:bool" = BoolTag
stringToTag "tag:yaml.org,2002:set" = SetTag
stringToTag "tag:yaml.org,2002:int" = IntTag
stringToTag "tag:yaml.org,2002:seq" = SeqTag
stringToTag "tag:yaml.org,2002:map" = MapTag
stringToTag "" = NoTag
stringToTag s = UriTag s

public export
data Event = EventStreamStart
           | EventStreamEnd
           | EventDocumentStart
           | EventDocumentEnd
           | EventAlias AnchorName
           | EventScalar String Tag Style Anchor
           | EventSequenceStart Anchor
           | EventSequenceEnd
           | EventMappingStart Anchor
           | EventMappingEnd

public export
Show Event where
  show EventStreamStart = "EventStreamStart"
  show EventStreamEnd = "EventStreamEnd"
  show EventDocumentStart = "EventDocumentStart"
  show EventDocumentEnd = "EventDocumentEnd"
  show (EventScalar str tag style anchor) = "EventScalar: " ++ show str -- TODO: show rest
  show (EventSequenceStart anchor) = "EventSequenceStart: " ++ show anchor
  show EventSequenceEnd = "EventSequenceEnd"
  show (EventMappingStart anchor) = "EventMappingStart: " ++ show anchor
  show EventMappingEnd = "EventMappingEnd"

safeString : IO String -> IO (Maybe String)
safeString s = do
  s' <- s
  isNull <- nullStr s'
  pure (toMaybe (not isNull) s')

getEvent : EventRaw -> IO (Maybe Event)
getEvent (MkEventRaw e) = do
  et <- foreign FFI_C "get_event_type" (Ptr -> IO Int) e
  if et == 0 then pure Nothing
  else Just <$> case et of
    -- YAML_STREAM_START_EVENT
    1 => pure EventStreamStart
    -- YAML_STREAM_END_EVENT,
    2 => pure EventStreamEnd
    -- YAML_DOCUMENT_START_EVENT,
    3 => pure EventDocumentStart
    -- YAML_DOCUMENT_END_EVENT,
    4 => pure EventDocumentEnd

    -- YAML_ALIAS_EVENT,
    5 => do
      anchor <- foreign FFI_C "get_alias_anchor" (Ptr -> IO String) e
      -- TODO: handle null?
      pure $ EventAlias anchor
    -- YAML_SCALAR_EVENT,
    6 => do
      value <- foreign FFI_C "get_scalar_value" (Ptr -> IO String) e
      putStrLn value
      -- TODO: handle null?
      ytag <- foreign FFI_C "get_scalar_tag" (Ptr -> IO String) e
      putStrLn ytag
      let tag = stringToTag ytag
      style <- intToStyle <$> foreign FFI_C "get_scalar_style" (Ptr -> IO Int) e
      anchor <- safeString $ foreign FFI_C "get_scalar_anchor" (Ptr -> IO String) e
      pure $ EventScalar value tag style anchor
    -- YAML_SEQUENCE_START_EVENT,
    7 => do
      anchor <- safeString $ foreign FFI_C "get_sequence_start_anchor" (Ptr -> IO String) e
      pure $ EventSequenceStart anchor
    -- YAML_SEQUENCE_END_EVENT,
    8 => pure EventSequenceEnd

    -- YAML_MAPPING_START_EVENT,
    9 => do
      anchor <- safeString $ foreign FFI_C "get_mapping_start_anchor" (Ptr -> IO String) e
      pure $ EventMappingStart anchor
    -- YAML_MAPPING_END_EVENT
    10 => pure EventMappingEnd

go : Parser -> List Event -> IO (List Event)
go p es = do
  Just e <- getEvent !(parser_parse p)
    | Nothing => pure es
  pure $ e :: !(go p es)

-- and now we throw the whole streaming interface out of the window...
export
decode : String -> IO (List Event)
decode s = do -- TODO: bracket
  p <- parser_initialize
  set_input_string p s
  res <- reverse <$> go p []
  parser_delete p
  pure res
