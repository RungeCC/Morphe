(* ::Package:: *)

BeginPackage["Typis`Error`", {"Wolfram`ErrorTools`"}];

CreateFailureTemplate::usage = "CreateFailureTemplate[tag, templateMessage, information]
produces a failure template which could receive template parameters and then return a
Failure object.";

FailureTemplate::usage = "Symbolic representation of FailureTemplate.";

CreateWrapFailureTemplate::usage = "CreateWrapFailureTemplate[tag, templateMessage, templateParameters]
produces a wrap failure template which could receive template parameters (shall contain a Failure ) and then return a wrapped Failure object.";

WrapFailureTemplate::usage = "Symbolic representation of WrapFailureTemplate.";

WrapRaisedWith::usage = "WrapRaisedWith[WrapFailureTemplate, associatedInfo]
Just like WrapRaised, but allow you to specify a WrapFailureTemplate to build the Failure object.";

RaiseConfirm::usage = "Raise a Failure resulted from Confirm expression.";

RaiseConfirmQuiet::usage = "Similar to RaiseConfirm, but Failure is produced by ConfirmQuiet.";

RaiseConfirmBy::usage = "Similar to RaiseConfirm, but Failure is produced by ConfirmBy.";

RaiseConfirmMatch::usage = "Similar to RaiseConfirm, but Failure is produced by ConfirmMatch.";

RaiseConfirmAssert::usage = "Similar to RaiseConfirm, but Failure is produced by ConfirmAssert.";

RaiseConfirmNoMessage::usage = "Similar to RaiseConfirmQuiet, but additionally requires that the result shall also be confirmed.";

RaiseConfirmNoThrow::usage = "Similar to RaiseConfirm, but additionally requires that there's nothing to be thrown out during the evaluation.
Notice we only handle tagged exceptions, since actually the non-tagged exceptions could not be distinguished from normal returned value before the value
reached the top level.";

ConfirmationFailureToString::usage = "Convert confirmation Failure into a string.

Second argument `template` shall be a function with signature _String -> _String.";

Begin["`Private`"];

CreateErrorType[InvalidStringTemplateError, {}];
CreateErrorType[AssociationKeyConflictError, {}];

SetAttributes[{
  RaiseConfirm, RaiseConfirmQuiet,
  RaiseConfirmBy, RaiseConfirmMatch,
  RaiseConfirmNoMessage, RaiseConfirmAssert,
  RaiseConfirmNoThrow},
  {HoldAllComplete}];

RaiseConfirm[expr_, info_ : Null] := Enclose[
  Confirm[expr, info, $RaiseConfirmTag],
  RaiseConfirm,
  $RaiseConfirmTag];

RaiseConfirmQuiet[expr_, mspec_ : All, info_ : Null] := Enclose[
  ConfirmQuiet[expr, mspec, info, $RaiseConfirmTag],
  RaiseConfirm,
  $RaiseConfirmTag];

RaiseConfirmNoMessage[expr_, mspec_ : All, infoc_ : Null, infom_ : Null] := Enclose[
  Confirm[ConfirmQuiet[expr, mspec, infom, $RaiseConfirmTag], infoc, $RaiseConfirmTag],
  RaiseConfirm,
  $RaiseConfirmTag];

RaiseConfirmBy[expr_, f_, info_ : Null] := Enclose[
  ConfirmBy[expr, f, info, $RaiseConfirmTag],
  RaiseConfirm,
  $RaiseConfirmTag];

RaiseConfirmBy[f_, info_: Null] := Function[expr, RaiseConfirmBy[expr, f, info], {HoldAllComplete}];

RaiseConfirmMatch[expr_, form_, info_ : Null] := Enclose[
  ConfirmMatch[expr, form, info, $RaiseConfirmTag],
  RaiseConfirm,
  $RaiseConfirmTag];

RaiseConfirmMatch[form_, info_: Null] := Function[
  expr,
  RaiseConfirmMatch[expr, form, info],
  {HoldAllComplete}];

RaiseConfirmAssert[cond_, info_ : Null] := Enclose[
  ConfirmAssert[cond, info, $RaiseConfirmTag],
  RaiseConfirm,
  $RaiseConfirmTag];

RaiseConfirmNoThrow[expr_, form_: HoldPattern[_], info_: Null] := RaiseConfirm @ Catch[
  expr, form,
  Function[{val, tag}, Failure["ConfirmationFailed",
    <|
      "MessageTemplate"   -> "\"`Expression`\" throws an exception (with tag \"`Tag`\" matches \"`Pattern`\") on evaluation.",
      "MessageParameters" -> <|
        "Expression" -> HoldForm @ expr,
        "Tag"        -> HoldForm @ tag,
        "Pattern"    -> HoldForm @ form
      |>,
      "ConfirmationType" -> "ConfirmNoThrow",
      "HeldExpression"   -> Hold[expr],
      "HeldThrownValue"  -> Hold[val],
      "HeldTag"          -> Hold[tag],
      "Information"      -> info
    |>
  ], {HoldAllComplete}]
];

hinted

ConfirmationFailureToString[
  fail_Failure, template_
] /; (fail @ "Tag" === "ConfirmationFailed") := Block[
  {
    type = fail @ "ConfirmationType"
  },
  template @ Switch[type,
    "Confirm", "confirmation failed.",
    "ConfirmBy", fail @ "Message",
    "ConfirmAssert",
      StringTemplate["assertion \"``\" failed."][HoldForm @@ fail @ "HeldTest"],
    "ConfirmQuiet",
      StringTemplate["message \"``\" emited."][HoldForm @@ fail @ "HeldMessageName"],
    "ConfirmNoThrow",
      StringTemplate["inner throws \"``\"."][HoldForm @@ fail @ "HeldThrownValue"],
    _, "unknown confirmation failure encountered."
  ]
];

(* Helper function, we do not expose it *)

RaiseConfirmNoConflictKeys[
  keys_List,
  assoc_?AssociationQ
] := WrapRaised[AssociationKeyConflictError,
  "Association \"`Association`\" contains keys in list \"`Keys`\", thus will conflict with existing keys.",
  <|"Association" -> assoc, "Keys" -> keys|>
] @ RaiseConfirmBy[assoc, KeyFreeQ[Alternatives @@ keys]];

RaiseConfirmNoConflictKeys[
  assoc0_?AssociationQ,
  assoc_?AssociationQ
] := RaiseConfirmNoConflictKeys[Keys@assoc0, assoc];

RaiseConfirmNoConflictKeys[a_] := Function[assoc, RaiseConfirmNoConflictKeys[a, assoc], {}];

CreateFailureTemplate[
  tag_? TagQ,
  temp_String,
  Optional[info_?AssociationQ, Association[]]
] := Handle[_Failure] @ With[
  {
    tempobj = WrapRaised[InvalidStringTemplateError,
      "String \"`Template`\" is not a valid string template.",
      <|"Template" -> temp|>
    ] @ RaiseConfirmNoMessage @ StringTemplate[temp],
    info0 = RaiseConfirmNoConflictKeys[{"MessageTemplate", "MessageParameters"}] @ info
  },
  FailureTemplate[<|"Tag" -> tag, "MessageTemplate" -> tempobj|> ~ Join ~ <|"Information" -> info0|>]
];

CreateFailureTemplate[
  tag_? TagQ,
  tempObj_TemplateObject,
  Optional[info_?AssociationQ, Association[]]
] := Handle[_Failure] @ With[
  {
    info0 = RaiseConfirmNoConflictKeys[{"MessageTemplate", "MessageParameters"}] @ info
  },
  FailureTemplate[<|"Tag" -> tag, "MessageTemplate" -> tempObj|> ~ Join ~ <|"Information" -> info0|>]
];

FailureTemplate[data_Association][
  param: Alternatives[_ ? AssociationQ, _List] : <| |>,
  Optional[extraInfo_?AssociationQ, Association[]]
] :=
  With[{info0 = Join[<|"MessageParameters" -> param, "MessageTemplate" -> data["MessageTemplate"]|>, data["Information"]]},
    Failure[
      data["Tag"], Join[info0, RaiseConfirmNoConflictKeys[info0] @ extraInfo]
    ]
  ] // Handle[_Failure];

CreateWrapFailureTemplate[
  tag    : _?TagQ,
  temp   : _String,
  params :  (({args___}) | (_? AssociationQ)) : <| |>,
  info   :  (_?AssociationQ)                  : <| |>
] := Handle[_Failure] @ With[
  {
    tempObj = WrapRaised[InvalidStringTemplateError,
      "String \"`Template`\" is not a valid string template.",
      <|"Template" -> temp|>
    ] @ RaiseConfirmNoMessage @ StringTemplate[temp],
    info0 = RaiseConfirmNoConflictKeys[{"MessageTemplate", "MessageParameters", "CausedBy"}] @ info
  },
  WrapFailureTemplate[<|"Tag" -> tag, "MessageTemplate" -> tempObj, "MessageParameters" -> params, "Information" -> info0|>]
];

CreateWrapFailureTemplate[
  tag    : _?TagQ,
  tempObj: _TemplateObject,
  params :  (({args___}) | (_? AssociationQ)) : Association[],
  info   :  (_?AssociationQ)                  : Association[]
] := Handle[_Failure]@With[
  {
    info0 = RaiseConfirmNoConflictKeys[{"MessageTemplate", "MessageParameters", "CausedBy"}] @ info
  },
  WrapFailureTemplate[<|"Tag" -> tag, "MessageTemplate" -> tempObj, "MessageParameters" -> params, "Information" -> info0|>]
];

(* create WrapFailureTemplate from a FailureTemplate *)
CreateWrapFailureTemplate[
  ft    : FailureTemplate[data_],
  params: (({args___}) | (_? AssociationQ)) : Association[],
  info  : (_?AssociationQ)                  : Association[]
] := Handle[_Failure] @ With[
  {
    info0 = RaiseConfirmNoConflictKeys[{
      "MessageTemplate", "CausedBy", "MessageParameters"
    }] @ RaiseConfirmNoConflictKeys[data["Information"]] @ info
  },
  WrapFailureTemplate[<|
    "Tag"               -> data["Tag"],
    "MessageTemplate"   -> data["MessageTemplate"],
    "MessageParameters" -> params,
    "Information"       -> Join[data["Information"], info0]|>]
];

WrapFailureTemplate[data_Association][caused_Failure] :=
  With[{info = Join[
    <|
      "MessageTemplate"   -> data["MessageTemplate"],
      "MessageParameters" -> data["MessageParameters"],
      "CausedBy"          -> caused
    |>,
    data["Information"]]},
    Failure[data["Tag"], info]
  ];

WrapFailureTemplate[data_Association][
  caused     : _Failure,
  extraParams: {___} | (_?AssociationQ),
  extraInfo  : (_?AssociationQ)        : Association[]
] := Handle[_Failure] @ With[
  {
    params = data["MessageParameters"],
    $ = Quiet[RaiseAssert[
      Head @ extraParams === Head @ (data["MessageParameters"]),
      "Early provided and later provided template parameters (`1` and `2`) shall either both be List or both be Association.",
      data["MessageParameters"], extraParams
    ], Wolfram`ErrorTools`V1`RaiseAssert::assertfail],
    info1 = Join[<|
        "MessageTemplate"   -> data["MessageTemplate"], 
        "MessageParameters" -> Join[data["MessageParameters"], extraParams],
        "CausedBy"          -> caused
      |>,
      data["Information"]]
  },
  Failure[
    data["Tag"], Join[info1, RaiseConfirmNoConflictKeys[info1] @ extraInfo]
  ]];

FailureTemplate /: MakeBoxes[f: FailureTemplate[data_Association], StandardForm] :=
  BoxForm`ArrangeSummaryBox[
    FailureTemplate,
    f,
    Null,
    BoxForm`SummaryItem /@ {{"Tag: ", data["Tag"]}, {"Message Template: ", data["MessageTemplate"]}},
    KeyValueMap[
      Function[{k, v}, BoxForm`SummaryItem[{k <> ": ", HoldForm @ v}]],
      KeyDrop[data, {"Tag", "MessageTemplate"}]], StandardForm
  ];

WrapFailureTemplate /: MakeBoxes[f: WrapFailureTemplate[data_Association], StandardForm] :=
  BoxForm`ArrangeSummaryBox[
    WrapFailureTemplate,
    f,
    Null,
    BoxForm`SummaryItem /@ {
      {"Tag: ", data["Tag"]},
      {"Message Template: ", data["MessageTemplate"]}
    },
    KeyValueMap[
      Function[{k, v}, BoxForm`SummaryItem[{k <> ": ", HoldForm @ v}]],
      KeyDrop[data, {"Tag", "MessageTemplate"}]], StandardForm
  ];

WrapRaisedWith[temp_WrapFailureTemplate, params : {___} | (_?AssociationQ)] := Handle[{
  err_Failure :> Raise[temp[err, params]]
}];

WrapRaisedWith[temp_WrapFailureTemplate] := Handle[{
  err_Failure :> Raise[temp[err]]
}];

End[];

EndPackage[];
