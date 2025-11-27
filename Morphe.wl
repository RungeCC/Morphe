BeginPackage["Morphe`"];

Morphe::usage = "Call external typst.";

$TypstPath::usage = "The path to the typst executable file.";
$TypstStringTemplates::usage = "String values used by option \"TypstStringTemplate\".

User defined template shall be a function with signature _String -> _String";
$TypstErrorHandlers::usage = "String values used by option \"TypstErrorHandler\".

User defined error handler shall be a function with signature {{type_String, message_String}...} -> _,
where type is either \"error\" or \"warning\".";

ClearMorpheCache::usage = "Clear the compiled cache of Morphe.";
GetMorpheCache::usage = "Get the compiled cache of Morphe.

Notice that this function will return the internal HashTable Object,
so you may need to use `Export` to dump it.";
SetMorpheCache::usage = "Set the compiled cache of Morphe.

This function receives a HashTable Object and copy it into the internal cache,
be careful that it will not validate the given HashTable Object.";

$MorpheVersion = "0.0.1";


(* TODOS:
+ check arguments
+ frontend completion supports
+ direct output Wolfram expression into typst
+ encoding typst types, elements in Wolfram language
+ validate typst flags
(* + cache outputs *)
(* + validate typst *)
(* + multi-pages output support *)
+ better error handling and cache for multi-pages mode
+ debug mode with timer support
*)

Begin["`Private`"];

<<Morphe`Error`;
<<Wolfram`ErrorTools`;

$Debug = False;

genDefaultCache := CreateDataStructure["HashTable",
  <|
    "TypstValidated"   -> False,
    "TypstInformation" -> <| |>,
    "Compilations"     -> CreateDataStructure["HashTable", <| |>]
  |>
];

cache = genDefaultCache; (* initialize cache *)

toAssociation[data_] := data //. {
  this: List[___Rule] :> With[{q = Association @ this}, q /; True]
};

ClearMorpheCache[opt_String] := (cache[
  "Insert",
  opt -> (genDefaultCache["Lookup", opt])
];);
ClearMorpheCache[All] := (cache = genDefaultCache;);

GetMorpheCache[] := cache;
SetMorpheCache[other_?(DataStructureQ[#, "HashTable"]&)] := (cache = other["Copy"];);

setCache[r: Verbatim[Rule][key_String, value_]] := cache["Insert", r];
getCache[key_String] := cache["Lookup", key];

setCompCache[r: Verbatim[Rule][key_, value_]] := cache["Lookup", "Compilations"]["Insert", r];
compCacheExistQ[key_] := cache["Lookup", "Compilations"]["KeyExistsQ", key];
getCompCache[key_] := cache["Lookup", "Compilations"]["Lookup", key];

validateTypst[path_String, clearCompCache: (_?BooleanQ) : True] := Block[
  {exit, output, error, res},
  res = Handle[_Failure] @ RaiseConfirmNoMessage @ RunProcess[{
    path, "info", "--format", "json"
  }];
  {exit, output, error} = Switch[
    res,
      _?AssociationQ,
        List @@ res,
      _,
        {-1, Null, res}
  ];
  error = Switch[error,
    _String,
      error,
    _Failure,
      ConfirmationFailureToString[
        error,
        ("error: " <> # <> "\n")&
      ]
  ];
  If[exit =!= 0,
    Return[fromUTF8 @ error];,
    (
      setCache["TypstValidated"   -> True];
      setCache["TypstInformation" -> toAssociation[ImportString[output, "JSON"]]];
      If[clearCompCache,
        ClearMorpheCache["Compilations"];
      ];
      Return[True];
    )
  ]
]

$TypstPath = "typst";

$TypstCompileFlags = {
  "--ignore-system-fonts",
  "--no-pdf-tags"
};

$TypstStringTemplates = <|
  "Equation" -> StringTemplate["#set page(width: auto, height: auto, margin: 0pt, fill: none)\n$ `` $"],
  "InlineEquation" -> StringTemplate["#set page(width: auto, height: auto, margin: 0pt, fill: none)\n$``$"],
  "Text" -> StringTemplate["#set page(width: auto, height: auto, margin: 0pt, fill: none)\n``"],
  "MiTeXEquation" -> (
    TemplateObject[{
      "#set page(width: auto, height: auto, margin: 0pt, fill: none)\n",
      "#import \"@preview/mitex:0.2.4\": mitex\n",
      "#mitex(`", TemplateSlot[1], "`)"},
      CombinerFunction  -> StringJoin,
      InsertionFunction -> TextString,
      MetaInformation   -> <| |>]
  ),
  "MiTeXInlineEquation" -> (
    TemplateObject[{
      "#set page(width: auto, height: auto, margin: 0pt, fill: none)\n",
      "#import \"@preview/mitex:0.2.4\": mi\n",
      "#mi(`", TemplateSlot[1], "`)"},
      CombinerFunction  -> StringJoin,
      InsertionFunction -> TextString,
      MetaInformation   -> <| |>]
  ),
  "MiTeXText" -> (
    TemplateObject[{
      "#set page(width: auto, height: auto, margin: 0pt, fill: none)\n",
      "#import \"@preview/mitex:0.2.4\": mitext\n",
      "#mitext(`", TemplateSlot[1], "`)"},
      CombinerFunction  -> StringJoin,
      InsertionFunction -> TextString,
      MetaInformation   -> <| |>]
  )
|>;

exactErrors[nowarn_?BooleanQ][str_String] := With[{
  splited = StringSplit[str,
    all: (
      (type: "error" | "warning")~~":"~~Shortest[title___]~~"\n"
    ) :> {type, all}
  ]},
  Apply[
    If[Length[{##}] == 2,
      {#1[[1]], #1[[2]] <> #2},
      {#1[[1]], #1[[2]]} (* If the last error is short, in this case. *)
    ] &
  ] /@ Partition[splited, UpTo[2]]
] // Select[If[nowarn, #[[1]] === "error", True]&];

fromUTF8[s_String] := FromCharacterCode[ToCharacterCode[s], "UTF-8"];

errorCellMaker[type_, text_] := With[{
    bgcolor = Switch[type, "error", LightRed, "warning", LightYellow, _, White],
    fgcolor = Switch[type, "error", Black, "warning", Black, _, Red]
  },
  Cell[ToString @ text, "Code",
    Editable   -> False,
    FontFamily -> "Fixed",
    FontSize   -> 12,
    Background -> bgcolor,
    FontColor  -> fgcolor
  ]
]

windowFunction[texts_List] := CreateDocument[
  errorCellMaker @@ #& /@ texts,
  WindowTitle   ->"Typst errors",
  WindowSize    -> {600, All},
  WindowMargins -> {{Automatic, 5}, {5, 5}}
];

printFunction[texts_List] := (CellPrint @* Apply[errorCellMaker]) /@ texts;

CreateErrorType[TypstError, {}];
CreateErrorType[TypstWarning, {}];
CreateErrorType[TypstErrors, {}];

typstError = CreateFailureTemplate[
  TypstError,
  "`Error`"
];

typstWarning = CreateFailureTemplate[
  TypstWarning,
  "`Warning`"
];

typstFailureList = CreateFailureTemplate[
  TypstErrors,
  "Fatal error occurs."
];

raiseFunction[texts_List] := (Apply[If[
  #1 == "error",
    typstError[<|"Error" -> #2|>],
    typstWarning[<|"Warning" -> #2|>]
]&] /@ texts) // typstFailureList[<| |>,
  <|
    "Errors"   -> Select[#1 @ "Tag" === TypstError&] @ #,
    "Warnings" -> Select[#1 @ "Tag" === TypstWarning&] @ #
  |>
]& // Raise;

Morphe::err = "`1`";
Morphe::wrn = "`1`";

messageFunction[texts_List] := Apply[(
  If[#1 == "error",
    Message[Morphe::err, #2],
    Message[Morphe::wrn, #2]
  ];
)&] /@ texts;

$TypstErrorHandlers = <|
  "Window"  -> windowFunction,
  "Print"   -> printFunction,
  "Message" -> messageFunction,
  "Raise"   -> raiseFunction
|>;

Options[Morphe] := {
  "TypstPath"           -> $TypstPath,
  "TypstStringTemplate" -> "Equation",
  "TypstCompileFlags"   -> $TypstCompileFlags,
  "TypstErrorHandler"   -> "Window",
  "Magnification"       -> 1,
  "NoCache"             -> False,
  "SuppressWarnings"    -> False,
  "MultiPages"     -> False
};

Morphe[str_String, opts: OptionsPattern[]] := Morphe[{str}, opts] // First;

Morphe[strs: {str___String}, opts: OptionsPattern[]] := Handle[_Failure] @ Block[
  {
    flags     = OptionValue["TypstCompileFlags"],
    tempopt   = OptionValue["TypstStringTemplate"],
    typst     = OptionValue["TypstPath"],
    hanopt    = OptionValue["TypstErrorHandler"],
    mag       = OptionValue[Magnification],
    validated = getCache["TypstValidated"],
    nocache   = TrueQ @ OptionValue["NoCache"],
    nowarn    = TrueQ @ OptionValue["SuppressWarnings"],
    multi     = TrueQ @ OptionValue["MultiPages"],
    allargs,
    errhandler,
    template,
    valid
  },
  template = Switch[tempopt,
    _String,
      $TypstStringTemplates[tempopt],
    _,
      tempopt
  ];
  errhandler = Switch[hanopt,
    _String,
      $TypstErrorHandlers[hanopt],
    _,
      hanopt
  ];
  allargs = Sequence[typst, flags, mag, errhandler, Not @ nocache, nowarn];
  fullstrs = template /@ strs;
  If[TrueQ @ validated || TrueQ @ (valid = validateTypst[typst]),
    If[Not @ multi,
      (fullstr |-> If[compCacheExistQ[{fullstr, flags}] && Not[nocache],
          getCompCache[{fullstr, flags}],
          iMorphe[fullstr, allargs]
      ]) /@ fullstrs,
      If[nocache,
        iMorpheMulti[fullstrs, allargs],
        Block[
          {
            missingIndices = Flatten @ Position[fullstrs, _?((Not @ compCacheExistQ[{#, flags}])&), Heads -> False],
            cachedIndices  = Flatten @ Position[fullstrs, _?(compCacheExistQ[{#, flags}]&), Heads -> False],
            cachedres,
            res
          },
          res = Thread[missingIndices -> iMorpheMulti[fullstrs[[missingIndices]], allargs]];
          cachedres = Thread[cachedIndices -> (getCompCache[{fullstrs[[#]], flags}]& /@ cachedIndices)];
          Last /@ SortBy[First][Join[res, cachedres]]
        ]
      ]
    ] (* Post-processing for image size and plot range *) \
      /. (HoldPattern[PlotRange -> _])    :> (PlotRange -> All) \
      /. (HoldPattern[ImageSize -> size_] :> (ImageSize -> mag * size)),
    errhandler[{
      {
        "error",
        "error: can not find valid typst executable, `typst info` complains:\n" <> \
        fromUTF8 @ valid <> \
        "=hint: confirm typst is in your path.\n" <> \
        "=hint: confirm typst version >= 0.14.0.\n\n"
      }
    }]
  ]
];

randName[n_] := StringJoin @ RandomChoice[
  Alphabet[]~Join~(Alphabet[]//ToUpperCase)~Join~(ToString/@Range[1,9]), n
];

iMorpheMulti[
  fullstrs: List[___String],
  typstpath_String,
  typstflags: List[___String],
  mag_?NumericQ,
  errorhandler_,
  caching_?BooleanQ,
  nowarn_?BooleanQ
]:= RaiseConfirmNoMessage @ Block[ (* $Failed and messages will be handled outside *)
  {
    dir = CreateDirectory[],
    fullstr = StringRiffle[fullstrs, "\n#pagebreak()\n"],
    exit, output, error, res,
    bytes,
    name = "output-" <> randName[10],
    imgs
  },
  bytes = ExportString[fullstr, "Text"];
  res = Handle[_Failure] @ RaiseConfirmNoMessage @ RunProcess[{
    typstpath, "compile",
    "-f", "svg",
    "--diagnostic-format", "human",
    Splice[typstflags],
    "-", name <> "-{p}.svg"
  }, All, bytes, ProcessDirectory -> dir];
  {exit, output, error} = Switch[res,
    (* If fatal error encountered, clear the caches. *)
    _?AssociationQ, List @@ res,
    _Failure, ClearMorpheCache["TypstValidated"]; {-1, Null,
      ConfirmationFailureToString[
        res, ("error: " <> # <> "\n" <> \
        "=hint: confirm typst is in your path.\n" <> \
        "=hint: confirm typst version >= 0.14.0.\n\n" &)
      ]
    }
  ];
  If[
    StringLength @ error > 0,
    error \
      // fromUTF8 \
      // exactErrors[nowarn] \
      // If[Length[#] > 0, errorhandler[#]]&;
    If[exit =!= 0,
      Return[$Failed];
    ];
  ];
  imgs = Import[
    FileNameJoin[{dir, name <> "-"<> ToString[#] <> ".svg"}],
    {"SVG", "Graphics"}
  ]& /@ Range[Length[fullstrs]];
  If[caching,
    (
      Apply[(setCompCache[{#1, flags} -> #2])&]
    ) /@ Transpose[{fullstrs, imgs}];
  ];
  imgs
];

iMorphe[
  fullstr_String,
  typstpath_String,
  typstflags: List[___String],
  mag_?NumericQ,
  errorhandler_,
  caching_?BooleanQ,
  nowarn_?BooleanQ
] := RaiseConfirmNoMessage @ Block[ (* $Failed and messages will be handled outside *)
  {
    exit, output, error, res,
    bytes = ExportString[fullstr, "Text"],
    img
  },
  res = Handle[_Failure] @ RaiseConfirmNoMessage @ RunProcess[{
    typstpath, "compile",
    "-f", "svg",
    "--diagnostic-format", "human",
    Splice[typstflags],
    "-", "-"
  }, All, bytes];
  {exit, output, error} = Switch[res,
    (* If fatal error encountered, clear the caches. *)
    _?AssociationQ, List @@ res,
    _Failure, ClearMorpheCache["TypstValidated"]; {-1, Null,
      ConfirmationFailureToString[
        res, ("error: " <> # <> "\n" <> \
        "=hint: confirm typst is in your path.\n" <> \
        "=hint: confirm typst version >= 0.14.0.\n\n" &)
      ]
    }
  ];
  If[
    StringLength @ error > 0,
    error \
      // fromUTF8 \
      // exactErrors[nowarn] \
      // If[Length[#] > 0, errorhandler[#]]&;
    If[exit =!= 0,
      Return[$Failed];
    ];
  ];
  img = ImportString[
    output, {"SVG", "Graphics"}, CharacterEncoding -> "UTF-8"
  ];
  If[caching,
    setCompCache[{fullstr, flags} -> img];
  ];
  img
];

End[];

EndPackage[];