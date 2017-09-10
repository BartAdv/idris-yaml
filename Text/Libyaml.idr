module Text.Libyaml

import CFFI

%include C "yaml.h"
%link C "libyaml.so"

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

set_input_file : Parser -> File -> IO ()
set_input_file (MkParser p) (FHandle f) = do
  foreign FFI_C "yaml_parser_set_input_file"
    (Ptr -> Ptr -> IO ()) p f

parser_parse : Parser -> EventRaw -> IO EventRaw
parser_parse (MkParser p) er@(MkEventRaw e) = do
  -- TODO: handle res
  foreign FFI_C "yaml_parser_parse" (Ptr -> Ptr -> IO ()) p e
  pure er

event_delete : EventRaw -> IO ()
event_delete (MkEventRaw e) =
  foreign FFI_C "yaml_event_delete" (Ptr -> IO ()) e

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

export
Eq Style where
  Plain        == Plain = True
  SingleQuoted == SingleQuoted = True
  DoubleQuoted == DoubleQuoted = True
  Literal      == Literal = True
  Folded       == Folded = True
  PlainNoTag   == PlainNoTag = True
  _ == _ = False

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

export
Eq Tag where
  StrTag      == StrTag = True
  FloatTag    == FloatTag = True
  NullTag     == NullTag = True
  BoolTag     == BoolTag = True
  SetTag      == SetTag = True
  IntTag      == IntTag = True
  SeqTag      == SeqTag = True
  MapTag      == MapTag = True
  (UriTag s1) == (UriTag s2) = s1 == s2
  NoTag       == NoTag  = True
  _ == _ = False

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

export
Eq Event where
  EventStreamStart           == EventStreamStart        = True
  EventStreamEnd             == EventStreamEnd          = True
  EventDocumentStart         == EventDocumentStart      = True
  EventDocumentEnd           == EventDocumentEnd        = True
  (EventAlias a1)            == (EventAlias a2)         = a1 == a2
  (EventScalar s1 t1 st1 a1) == (EventScalar s2 t2 st2 a2) =
    s1 == s2 && t1 == t2 && st1 == st2 && a1 == a2
  (EventSequenceStart a1)    == (EventSequenceStart a2) = a1 == a2
  EventSequenceEnd           == EventSequenceEnd        = True
  (EventMappingStart a1)     == (EventMappingStart a2)  = a1 == a2
  EventMappingEnd            == EventMappingEnd         = True
  _ == _ = False

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
      -- TODO: handle null?
      ytag <- foreign FFI_C "get_scalar_tag" (Ptr -> IO String) e
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

go : Parser -> EventRaw -> List Event -> IO (List Event)
go p er es = do
  Just e <- getEvent !(parser_parse p er)
    | Nothing => pure es
  pure $ e :: !(go p er es)

-- and now we throw the whole streaming interface out of the window...
export
decode : String -> IO (List Event)
decode s = do -- TODO: bracket
  p <- parser_initialize
  -- reusing one event here...
  er <- MkEventRaw <$> malloc eventSize
  set_input_string p s
  res <- go p er []
  event_delete er
  parser_delete p
  pure res

export
decodeFile : String -> IO (Either FileError (List Event))
decodeFile fname = do -- TODO: bracket, reuse init/cleanup with above
  p <- parser_initialize
  Right file <- fopen fname "rb"
    | Left err => pure (Left err)
  set_input_file p file
  er <- MkEventRaw <$> malloc eventSize
  res <- go p er []
  closeFile file
  event_delete er
  -- TODO: figure out why it can't cleaned up after reading file
  -- parser_delete p
  pure (Right res)
