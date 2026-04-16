# BuiltinInputPatterns.wl - Manual Coverage Todo

**Target:** Manual `$identityOverrides` entries for every System function with SyntaxInformation.

**Total functions with SyntaxInformation:** 5,343  
**Already covered in `$identityOverrides`:** 174  
**Covered by naming convention (`*Q`, `String*`, `*List`, `*Count`, `*Plot`, `*Chart`):** ~492  
**Remaining to fill manually:** ~4,713  

## Type Vocabulary

Arg types: `String` `Integer` `Real` `Complex` `Rational` `Image` `Audio` `Video` `Association` `List` `Graph` `SparseArray` `Symbol` `Rule` `RuleDelayed` `Quantity` `_?BooleanQ` `None` (untyped), variadic: `Type...` or `...`

Return types: `_String` `_Integer` `_Real` `_Complex` `_Rational` `_Image` `_Audio` `_Video` `_Association` `_List` `_Graph` `_SparseArray` `_Quantity` `_?BooleanQ` `_?NumericQ` `_[1]` (passthrough first arg) `_Graphics` `_Graphics3D` `None`

## Progress Summary

| Category | Total | Status |
|----------|-------|--------|
| Already in `$identityOverrides` | 174 | Complete |
| Naming conventions | ~492 | Auto-handled |
| [Image Processing](#image-processing) | 81 | Pending |
| [Audio Processing](#audio-processing) | 49 | Pending |
| [Video Processing](#video-processing) | 29 | Pending |
| [Graph and Network Theory](#graph-and-network-theory) | 68 | Pending |
| [Date and Time Functions](#date-and-time-functions) | 46 | Pending |
| [File System and IO](#file-system-and-io) | 65 | Pending |
| [Association and Key Functions](#association-and-key-functions) | 19 | Pending |
| [Matrix Operations](#matrix-operations) | 75 | Pending |
| [Polynomial Mathematics](#polynomial-mathematics) | 32 | Pending |
| [Machine Learning](#machine-learning) | 52 | Pending |
| [Number and Digit Functions](#number-and-digit-functions) | 51 | Pending |
| [Random Functions](#random-functions) | 23 | Pending |
| [Region and Geometry](#region-and-geometry) | 94 | Pending |
| [Color Functions](#color-functions) | 36 | Pending |
| General Functions (80 batches) | 3,992 | Pending |

---

## Already Covered (174 functions in `$identityOverrides`)

`Abs`, `Abs2`, `Accumulate`, `And`, `ArcCos`, `ArcCosh`, `ArcCot`, `ArcCoth`, `ArcCsc`, `ArcSec`, `ArcSin`, `ArcSinh`, `ArcTan`, `ArcTanh`, `Arg`, `ArrayQ`, `Binomial`, `BitAnd`, `BitNot`, `BitOr`, `BitShiftLeft`, `BitShiftRight`, `BitXor`, `BooleanQ`, `Ceiling`, `Chop`, `Clip`, `Confirm`, `ConfirmAssert`, `ConfirmBy`, `ConfirmMatch`, `ConfirmQuiet`, `Conjugate`, `ContainsAll`, `ContainsAny`, `ContainsNone`, `Cos`, `Cosh`, `Cot`, `Coth`, `Covariance`, `Cross`, `Csc`, `Csch`, `CumulativeSum`, `Det`, `DiagonalMatrixQ`, `Differences`, `DivisorSigma`, `Dot`, `Echo`, `EchoEvaluation`, `EchoTiming`, `Eigenvalues`, `Eigenvectors`, `Enclose`, `Equal`, `EulerPhi`, `EvenQ`, `ExactNumberQ`, `Exp`, `Factorial`, `Factorial2`, `Fibonacci`, `FindMaximum`, `FindMinimum`, `FindRoot`, `Floor`, `FractionalPart`, `FreeQ`, `GCD`, `Greater`, `GreaterEqual`, `HermitianMatrixQ`, `Identity`, `If`, `Im`, `Implies`, `InexactNumberQ`, `Inner`, `IntegerPart`, `IntegerQ`, `Inverse`, `JacobiSymbol`, `KroneckerSymbol`, `LCM`, `Less`, `LessEqual`, `LinearSolve`, `Log`, `Log10`, `Log2`, `LucasL`, `MatchQ`, `MatrixQ`, `MatrixRank`, `Max`, `Maximize`, `Mean`, `Median`, `MemberQ`, `Min`, `MinMax`, `Minimize`, `Mod`, `MoebiusMu`, `N`, `NMaximize`, `NMinimize`, `NSolve`, `Nand`, `NegativeQ`, `NonNegativeQ`, `NonPositiveQ`, `Nor`, `Norm`, `Not`, `NullSpace`, `NumberQ`, `NumericQ`, `OddQ`, `Or`, `OrderedQ`, `Outer`, `PartitionsP`, `Plus`, `PositiveQ`, `Power`, `Prime`, `PrimeNu`, `PrimeOmega`, `PrimeQ`, `Quotient`, `RandomChoice`, `RandomComplex`, `RandomInteger`, `RandomReal`, `Rationalize`, `Ratios`, `Re`, `RealValuedNumberQ`, `Reduce`, `Round`, `RowReduce`, `SameQ`, `Sec`, `Sech`, `Sign`, `Sin`, `Sinh`, `Solve`, `Sqrt`, `SquareMatrixQ`, `StandardDeviation`, `StringContainsQ`, `StringEndsQ`, `StringFreeQ`, `StringMatchQ`, `StringStartsQ`, `SubsetQ`, `Switch`, `SymmetricMatrixQ`, `Tan`, `Tanh`, `Times`, `Total`, `Tr`, `TrueQ`, `Unequal`, `UnsameQ`, `Variance`, `VectorQ`, `Which`, `Xor`

---

## Image Processing

**Count:** 81  **Status:** Pending

- [x] `Image`
- [x] `Image3D`
- [x] `Image3DProjection`
- [x] `Image3DSlices`
- [x] `ImageAccumulate`
- [x] `ImageAdd`
- [x] `ImageAdjust`
- [x] `ImageAlign`
- [x] `ImageApply`
- [x] `ImageApplyIndexed`
- [x] `ImageAspectRatio`
- [x] `ImageAssemble`
- [x] `ImageAugmentationLayer`
- [x] `ImageBoundingBoxes`
- [x] `ImageCapture`
- [x] `ImageCases`
- [x] `ImageChannels`
- [x] `ImageClip`
- [x] `ImageCollage`
- [x] `ImageColorSpace`
- [x] `ImageCompose`
- [x] `ImageContents`
- [x] `ImageConvolve`
- [x] `ImageCooccurrence`
- [x] `ImageCorners`
- [x] `ImageCorrelate`
- [x] `ImageCorrespondingPoints`
- [x] `ImageCrop`
- [x] `ImageData`
- [x] `ImageDeconvolve`
- [x] `ImageDemosaic`
- [x] `ImageDifference`
- [x] `ImageDimensions`
- [x] `ImageDisplacements`
- [x] `ImageDistance`
- [x] `ImageEffect`
- [x] `ImageExposureCombine`
- [x] `ImageFeatureTrack`
- [x] `ImageFileApply`
- [x] `ImageFileFilter`
- [x] `ImageFileScan`
- [x] `ImageFilter`
- [x] `ImageFocusCombine`
- [x] `ImageForestingComponents`
- [x] `ImageForwardTransformation`
- [x] `ImageGraphics`
- [x] `ImageHistogram`
- [x] `ImageIdentify`
- [x] `ImageKeypoints`
- [x] `ImageLevels`
- [x] `ImageLines`
- [x] `ImageMarker`
- [x] `ImageMeasurements`
- [x] `ImageMesh`
- [x] `ImageMultiply`
- [x] `ImagePad`
- [x] `ImagePartition`
- [x] `ImagePeriodogram`
- [x] `ImagePerspectiveTransformation`
- [x] `ImagePosition`
- [x] `ImagePyramid`
- [x] `ImagePyramidApply`
- [x] `ImageRecolor`
- [x] `ImageReflect`
- [x] `ImageResize`
- [x] `ImageRestyle`
- [x] `ImageRotate`
- [x] `ImageSaliencyFilter`
- [x] `ImageScaled`
- [x] `ImageScan`
- [x] `ImageSegmentationComponents`
- [x] `ImageSegmentationFilter`
- [x] `ImageStitch`
- [x] `ImageSubtract`
- [x] `ImageSynthesize`
- [x] `ImageTake`
- [x] `ImageTransformation`
- [x] `ImageTrim`
- [x] `ImageType`
- [x] `ImageValue`
- [x] `ImageValuePositions`

---

## Audio Processing

**Count:** 49  **Status:** Pending

- [x] `Audio`
- [x] `AudioAmplify`
- [x] `AudioAnnotate`
- [x] `AudioAnnotationLookup`
- [x] `AudioBlockMap`
- [x] `AudioCapture`
- [x] `AudioChannelCombine`
- [x] `AudioChannelMix`
- [x] `AudioChannels`
- [x] `AudioChannelSeparate`
- [x] `AudioData`
- [x] `AudioDelay`
- [x] `AudioDelete`
- [x] `AudioDistance`
- [x] `AudioFade`
- [x] `AudioFrequencyShift`
- [x] `AudioGenerator`
- [x] `AudioIdentify`
- [x] `AudioInsert`
- [x] `AudioIntervals`
- [x] `AudioJoin`
- [x] `AudioLength`
- [x] `AudioLocalMeasurements`
- [x] `AudioLoudness`
- [x] `AudioMeasurements`
- [x] `AudioNormalize`
- [x] `AudioOverlay`
- [x] `AudioPad`
- [x] `AudioPan`
- [x] `AudioPartition`
- [x] `AudioPause`
- [x] `AudioPitchShift`
- [x] `AudioPlay`
- [x] `AudioRecord`
- [x] `AudioReplace`
- [x] `AudioResample`
- [x] `AudioReverb`
- [x] `AudioReverse`
- [x] `AudioSampleRate`
- [x] `AudioSpectralMap`
- [x] `AudioSpectralTransformation`
- [x] `AudioSplit`
- [x] `AudioStop`
- [x] `AudioStream`
- [x] `AudioStreams`
- [x] `AudioTimeStretch`
- [x] `AudioTrackApply`
- [x] `AudioTrim`
- [x] `AudioType`

---

## Video Processing

**Count:** 29  **Status:** Pending

- [x] `Video`
- [x] `VideoCapture`
- [x] `VideoCombine`
- [x] `VideoDelete`
- [x] `VideoExtractFrames`
- [x] `VideoExtractTracks`
- [x] `VideoFrameFold`
- [x] `VideoFrameMap`
- [x] `VideoGenerator`
- [x] `VideoInsert`
- [x] `VideoIntervals`
- [x] `VideoJoin`
- [x] `VideoMap`
- [x] `VideoMapTimeSeries`
- [x] `VideoObjectTracking`
- [x] `VideoPause`
- [x] `VideoPlay`
- [x] `VideoRecord`
- [x] `VideoReplace`
- [x] `VideoScreenCapture`
- [x] `VideoSplit`
- [x] `VideoStabilize`
- [x] `VideoStop`
- [x] `VideoStream`
- [x] `VideoStreams`
- [x] `VideoTimeStretch`
- [x] `VideoTranscode`
- [x] `VideoTranscribe`
- [x] `VideoTrim`

---

## Graph and Network Theory

**Count:** 68  **Status:** Pending

- [x] `Graph`
- [x] `Graph3D`
- [x] `GraphAssortativity`
- [x] `GraphAutomorphismGroup`
- [x] `GraphCenter`
- [x] `GraphComplement`
- [x] `GraphData`
- [x] `GraphDensity`
- [x] `GraphDiameter`
- [x] `GraphDifference`
- [x] `GraphDisjointUnion`
- [x] `GraphDistance`
- [x] `GraphDistanceMatrix`
- [x] `GraphEmbedding`
- [x] `GraphHub`
- [x] `Graphics`
- [x] `Graphics3D`
- [x] `GraphicsArray`
- [x] `GraphicsColumn`
- [x] `GraphicsComplex`
- [x] `GraphicsGrid`
- [x] `GraphicsGroup`
- [x] `GraphicsRow`
- [x] `GraphIntersection`
- [x] `GraphJoin`
- [x] `GraphLinkEfficiency`
- [x] `GraphPeriphery`
- [x] `GraphPower`
- [x] `GraphProduct`
- [x] `GraphPropertyDistribution`
- [x] `GraphRadius`
- [x] `GraphReciprocity`
- [x] `GraphSum`
- [x] `GraphTree`
- [x] `GraphUnion`
- [x] `VertexAdd`
- [x] `VertexChromaticNumber`
- [x] `VertexComponent`
- [x] `VertexConnectivity`
- [x] `VertexContract`
- [x] `VertexCorrelationSimilarity`
- [x] `VertexCosineSimilarity`
- [x] `VertexDegree`
- [x] `VertexDelete`
- [x] `VertexDiceSimilarity`
- [x] `VertexEccentricity`
- [x] `VertexInComponent`
- [x] `VertexInComponentGraph`
- [x] `VertexInDegree`
- [x] `VertexIndex`
- [x] `VertexJaccardSimilarity`
- [x] `VertexOutComponent`
- [x] `VertexOutComponentGraph`
- [x] `VertexOutDegree`
- [x] `VertexReplace`
- [x] `EdgeAdd`
- [x] `EdgeBetweennessCentrality`
- [x] `EdgeChromaticNumber`
- [x] `EdgeConnectivity`
- [x] `EdgeContract`
- [x] `EdgeCycleMatrix`
- [x] `EdgeDelete`
- [x] `EdgeDetect`
- [x] `EdgeForm`
- [x] `EdgeIndex`
- [x] `EdgeRules`
- [x] `EdgeTaggedGraph`
- [x] `EdgeTags`

---

## Date and Time Functions

**Count:** 46  **Status:** Pending

- [x] `Date`
- [x] `DateBounds`
- [x] `Dated`
- [x] `DateDifference`
- [x] `DateDistribution`
- [x] `DatedUnit`
- [x] `DateHistogram`
- [x] `DateInterval`
- [x] `DateObject`
- [x] `DatePattern`
- [x] `DatePlus`
- [x] `DateRange`
- [x] `DateScale`
- [x] `DateSelect`
- [x] `DateString`
- [x] `DateValue`
- [x] `TimeConstrained`
- [x] `TimeDistribution`
- [x] `TimeObject`
- [x] `TimeRemaining`
- [x] `TimesBy`
- [x] `TimeSeries`
- [x] `TimeSeriesAggregate`
- [x] `TimeSeriesEvents`
- [x] `TimeSeriesForecast`
- [x] `TimeSeriesInsert`
- [x] `TimeSeriesInvertibility`
- [x] `TimeSeriesMap`
- [x] `TimeSeriesMapThread`
- [x] `TimeSeriesModel`
- [x] `TimeSeriesModelFit`
- [x] `TimeSeriesResample`
- [x] `TimeSeriesRescale`
- [x] `TimeSeriesShift`
- [x] `TimeSeriesStructure`
- [x] `TimeSeriesSummary`
- [x] `TimeSeriesThread`
- [x] `TimeSeriesWindow`
- [x] `TimeSystemConvert`
- [x] `TimeUsed`
- [x] `TimeValue`
- [x] `TimeWarpingCorrespondence`
- [x] `TimeWarpingDistance`
- [x] `TimeZone`
- [x] `TimeZoneConvert`
- [x] `TimeZoneOffset`

---

## File System and IO

**Count:** 65  **Status:** Pending

- [x] `AbsoluteFileName`
- [x] `CopyFile`
- [x] `CreateFile`
- [x] `DecryptFile`
- [x] `DeleteFile`
- [x] `EncryptFile`
- [x] `ExpandFileName`
- [x] `File`
- [x] `FileBaseName`
- [x] `FileConvert`
- [x] `FileDate`
- [x] `FileExtension`
- [x] `FileFormat`
- [x] `FileFormatProperties`
- [x] `FileHash`
- [x] `FileNameDepth`
- [x] `FileNameDrop`
- [x] `FileNameJoin`
- [x] `FileNames`
- [x] `FileNameSetter`
- [x] `FileNameSplit`
- [x] `FileNameTake`
- [x] `FilePrint`
- [x] `FileSize`
- [x] `FileSystemMap`
- [x] `FileSystemScan`
- [x] `FileSystemTree`
- [x] `FileTemplate`
- [x] `FileTemplateApply`
- [x] `FileType`
- [x] `FindFile`
- [x] `GenerateFileSignature`
- [x] `ImageFileApply`
- [x] `ImageFileFilter`
- [x] `ImageFileScan`
- [x] `NotebookFileName`
- [x] `RemoteFile`
- [x] `RenameFile`
- [x] `SetFileDate`
- [x] `SetFileFormatProperties`
- [x] `ToFileName`
- [x] `VerifyFileSignature`
- [x] `CloudDirectory`
- [x] `CopyDirectory`
- [x] `CreateDirectory`
- [x] `DeleteDirectory`
- [x] `Directory`
- [x] `DirectoryName`
- [x] `DirectoryStack`
- [x] `NotebookDirectory`
- [x] `PacletDirectoryLoad`
- [x] `PacletDirectoryUnload`
- [x] `ParentDirectory`
- [x] `RenameDirectory`
- [x] `ResetDirectory`
- [x] `SetCloudDirectory`
- [x] `SetDirectory`
- [x] `Import`
- [x] `ImportByteArray`
- [x] `ImportedObject`
- [x] `ImportString`
- [x] `Export`
- [x] `ExportByteArray`
- [x] `ExportForm`
- [x] `ExportString`

---

## Association and Key Functions

**Count:** 19  **Status:** Pending

- [x] `AssociateTo`
- [x] `Association`
- [x] `AssociationComap`
- [x] `AssociationMap`
- [x] `AssociationThread`
- [x] `Key`
- [x] `KeyComplement`
- [x] `KeyDrop`
- [x] `KeyDropFrom`
- [x] `KeyIntersection`
- [x] `KeyMap`
- [x] `Keys`
- [x] `KeySelect`
- [x] `KeySort`
- [x] `KeySortBy`
- [x] `KeyTake`
- [x] `KeyUnion`
- [x] `KeyValueMap`
- [x] `KeyValuePattern`

---

## Matrix Operations

**Count:** 75  **Status:** Pending

- [x] `AdjacencyMatrix`
- [x] `BlockDiagonalMatrix`
- [x] `BlockLowerTriangularMatrix`
- [x] `BlockUpperTriangularMatrix`
- [x] `BoxMatrix`
- [x] `CauchyMatrix`
- [x] `CircularOrthogonalMatrixDistribution`
- [x] `CircularQuaternionMatrixDistribution`
- [x] `CircularRealMatrixDistribution`
- [x] `CircularSymplecticMatrixDistribution`
- [x] `CircularUnitaryMatrixDistribution`
- [x] `CompanionMatrix`
- [x] `ControllabilityMatrix`
- [x] `CrossMatrix`
- [x] `DesignMatrix`
- [x] `DiagonalMatrix`
- [x] `DiamondMatrix`
- [x] `DiskMatrix`
- [x] `DistanceMatrix`
- [x] `EdgeCycleMatrix`
- [x] `EulerMatrix`
- [x] `FindMatrixGameStrategies`
- [x] `FourierDCTMatrix`
- [x] `FourierDSTMatrix`
- [x] `FourierMatrix`
- [x] `GaborMatrix`
- [x] `GaussianMatrix`
- [x] `GaussianOrthogonalMatrixDistribution`
- [x] `GaussianSymplecticMatrixDistribution`
- [x] `GaussianUnitaryMatrixDistribution`
- [x] `GraphDistanceMatrix`
- [x] `HadamardMatrix`
- [x] `HankelMatrix`
- [x] `HermitianMatrix`
- [x] `HilbertMatrix`
- [x] `IdentityMatrix`
- [x] `IncidenceMatrix`
- [x] `InverseWishartMatrixDistribution`
- [x] `JordanMatrix`
- [x] `KirchhoffMatrix`
- [x] `LowerTriangularMatrix`
- [x] `MatrixExp`
- [x] `MatrixForm`
- [x] `MatrixFunction`
- [x] `MatrixGame`
- [x] `MatrixGamePayoff`
- [x] `MatrixLog`
- [x] `MatrixMinimalPolynomial`
- [x] `MatrixNormalDistribution`
- [x] `MatrixPolynomialValue`
- [x] `MatrixPower`
- [x] `MatrixPropertyDistribution`
- [x] `MatrixSymbol`
- [x] `MatrixTDistribution`
- [x] `ObservabilityMatrix`
- [x] `OrthogonalMatrix`
- [x] `OutputControllabilityMatrix`
- [x] `PauliMatrix`
- [x] `PermutationMatrix`
- [x] `ReflectionMatrix`
- [x] `RollPitchYawMatrix`
- [x] `RotationMatrix`
- [x] `SavitzkyGolayMatrix`
- [x] `ScalingMatrix`
- [x] `ShearingMatrix`
- [x] `ShenCastanMatrix`
- [x] `SymmetricMatrix`
- [x] `ToeplitzMatrix`
- [x] `TransformationMatrix`
- [x] `UnitaryMatrix`
- [x] `UpperTriangularMatrix`
- [x] `VandermondeMatrix`
- [x] `VerifyMatrixGameStrategy`
- [x] `WeightedAdjacencyMatrix`
- [x] `WishartMatrixDistribution`

---

## Polynomial Mathematics

**Count:** 32  **Status:** Pending

- [x] `AlgebraicNumberPolynomial`
- [x] `AugmentedSymmetricPolynomial`
- [x] `CharacteristicPolynomial`
- [x] `ChromaticPolynomial`
- [x] `CycleIndexPolynomial`
- [x] `FlowPolynomial`
- [x] `InterpolatingPolynomial`
- [x] `MatrixMinimalPolynomial`
- [x] `MatrixPolynomialValue`
- [x] `MinimalPolynomial`
- [x] `NonCommutativePolynomialReduce`
- [x] `NonCommutativePolynomialReduction`
- [x] `PolynomialExtendedGCD`
- [x] `PolynomialGCD`
- [x] `PolynomialHermiteDecomposition`
- [x] `PolynomialHermiteReduce`
- [x] `PolynomialLCM`
- [x] `PolynomialMod`
- [x] `PolynomialModel`
- [x] `PolynomialQuotient`
- [x] `PolynomialQuotientRemainder`
- [x] `PolynomialReduce`
- [x] `PolynomialReduction`
- [x] `PolynomialRemainder`
- [x] `Polynomials`
- [x] `PolynomialSmithDecomposition`
- [x] `PolynomialSmithReduce`
- [x] `PowerSymmetricPolynomial`
- [x] `SubresultantPolynomialRemainders`
- [x] `SubresultantPolynomials`
- [x] `SymmetricPolynomial`
- [x] `TuttePolynomial`

---

## Machine Learning

**Count:** 52  **Status:** Pending

- [x] `ClassifierFunction`
- [x] `ClassifierInformation`
- [x] `ClassifierMeasurements`
- [x] `ClassifierMeasurementsObject`
- [x] `LayeredGraph`
- [x] `LayeredGraph3D`
- [x] `NetAppend`
- [x] `NetArray`
- [x] `NetArrayLayer`
- [x] `NetBidirectionalOperator`
- [x] `NetChain`
- [x] `NetDecoder`
- [x] `NetDelete`
- [x] `NetDrop`
- [x] `NetEncoder`
- [x] `NetExternalObject`
- [x] `NetExtract`
- [x] `NetFlatten`
- [x] `NetFoldOperator`
- [x] `NetGANOperator`
- [x] `NetGraph`
- [x] `NetInformation`
- [x] `NetInitialize`
- [x] `NetInsert`
- [x] `NetInsertSharedArrays`
- [x] `NetJoin`
- [x] `NetMapOperator`
- [x] `NetMapThreadOperator`
- [x] `NetMeasurements`
- [x] `NetModel`
- [x] `NetNestOperator`
- [x] `NetPairEmbeddingOperator`
- [x] `NetPort`
- [x] `NetPortGradient`
- [x] `NetPrepend`
- [x] `NetRename`
- [x] `NetReplace`
- [x] `NetReplacePart`
- [x] `NetSharedArray`
- [x] `NetStateObject`
- [x] `NetTake`
- [x] `NetTrain`
- [x] `NetTrainResultsObject`
- [x] `NetUnfold`
- [x] `NetworkPacketCapture`
- [x] `NetworkPacketRecording`
- [x] `NetworkPacketTrace`
- [x] `PredictorFunction`
- [x] `PredictorInformation`
- [x] `PredictorMeasurements`
- [x] `PredictorMeasurementsObject`
- [x] `SequencePredictorFunction`

---

## Number and Digit Functions

**Count:** 51  **Status:** Pending

- [x] `AlgebraicNumber`
- [x] `AlgebraicNumberDenominator`
- [x] `AlgebraicNumberNorm`
- [x] `AlgebraicNumberPolynomial`
- [x] `AlgebraicNumberTrace`
- [x] `AlternatingHarmonicNumber`
- [x] `CatalanNumber`
- [x] `ChampernowneNumber`
- [x] `DigitalSignature`
- [x] `DigitSum`
- [x] `EdgeChromaticNumber`
- [x] `FrobeniusNumber`
- [x] `FromDigits`
- [x] `FromLetterNumber`
- [x] `FromLunationNumber`
- [x] `GenerateDigitalSignature`
- [x] `HarmonicNumber`
- [x] `HyperHarmonicNumber`
- [x] `IntegerDigits`
- [x] `IntegerExponent`
- [x] `IntegerLength`
- [x] `IntegerName`
- [x] `IntegerPartitions`
- [x] `IntegerReverse`
- [x] `IntegerString`
- [x] `LetterNumber`
- [x] `LunationNumber`
- [x] `MorphologicalEulerNumber`
- [x] `MultipleHarmonicNumber`
- [x] `NumberCompose`
- [x] `NumberDecompose`
- [x] `NumberDigit`
- [x] `NumberExpand`
- [x] `NumberFieldClassNumber`
- [x] `NumberFieldDiscriminant`
- [x] `NumberFieldFundamentalUnits`
- [x] `NumberFieldIntegralBasis`
- [x] `NumberFieldNormRepresentatives`
- [x] `NumberFieldRegulator`
- [x] `NumberFieldRootsOfUnity`
- [x] `NumberFieldSignature`
- [x] `NumberForm`
- [x] `PerfectNumber`
- [x] `PolygonalNumber`
- [x] `RealAbs`
- [x] `RealDigits`
- [x] `RealExponent`
- [x] `RealSign`
- [x] `ToNumberField`
- [x] `VerifyDigitalSignature`
- [x] `VertexChromaticNumber`

---

## Random Functions

**Count:** 23  **Status:** Pending

- [x] `Random`
- [x] `RandomArrayLayer`
- [x] `RandomColor`
- [x] `RandomDate`
- [x] `RandomEntity`
- [x] `RandomFunction`
- [x] `RandomGeneratorState`
- [x] `RandomGeoPosition`
- [x] `RandomGraph`
- [x] `RandomImage`
- [x] `RandomInstance`
- [x] `RandomPermutation`
- [x] `RandomPoint`
- [x] `RandomPointConfiguration`
- [x] `RandomPolygon`
- [x] `RandomPolyhedron`
- [x] `RandomPrime`
- [x] `RandomSample`
- [x] `RandomTime`
- [x] `RandomTree`
- [x] `RandomVariate`
- [x] `RandomWalkProcess`
- [x] `RandomWord`

---

## Region and Geometry

**Count:** 94  **Status:** Pending

- [x] `AdjacentMeshCells`
- [x] `ArrayMesh`
- [x] `Ball`
- [x] `BooleanRegion`
- [x] `BoundaryDiscretizeRegion`
- [x] `BoundaryMesh`
- [x] `BoundaryMeshRegion`
- [x] `BoundingRegion`
- [x] `CanonicalizeRegion`
- [x] `CantorMesh`
- [x] `ConcaveHullMesh`
- [x] `Cone`
- [x] `ConicHullRegion`
- [x] `ConnectedMeshComponents`
- [x] `ConvexHullMesh`
- [x] `ConvexHullRegion`
- [x] `CSGRegion`
- [x] `CSGRegionTree`
- [x] `DelaunayMesh`
- [x] `DimensionalMeshComponents`
- [x] `DiscretizeRegion`
- [x] `EmptyRegion`
- [x] `FindMeshDefects`
- [x] `FindRegionTransform`
- [x] `FullRegion`
- [x] `GeoBoundsRegion`
- [x] `GeoBoundsRegionBoundary`
- [x] `GeoVisibleRegion`
- [x] `GeoVisibleRegionBoundary`
- [x] `GradientFittedMesh`
- [x] `HighlightMesh`
- [x] `HighlightRegion`
- [x] `ImageMesh`
- [x] `ImplicitRegion`
- [x] `InverseTransformedRegion`
- [x] `MengerMesh`
- [x] `MeshCellIndex`
- [x] `MeshCells`
- [x] `MeshConnectivityGraph`
- [x] `MeshCoordinates`
- [x] `MeshPrimitives`
- [x] `MeshRegion`
- [x] `MoleculeMesh`
- [x] `NearestMeshCells`
- [x] `ParametricRegion`
- [x] `ReconstructionMesh`
- [x] `Region`
- [x] `RegionBinarize`
- [x] `RegionBoundary`
- [x] `RegionBounds`
- [x] `RegionCentroid`
- [x] `RegionCongruent`
- [x] `RegionConvert`
- [x] `RegionDifference`
- [x] `RegionDilation`
- [x] `RegionDimension`
- [x] `RegionDisjoint`
- [x] `RegionDistance`
- [x] `RegionDistanceFunction`
- [x] `RegionEmbeddingDimension`
- [x] `RegionEqual`
- [x] `RegionErosion`
- [x] `RegionFarthestDistance`
- [x] `RegionFit`
- [x] `RegionGaussianCurvature`
- [x] `RegionHausdorffDistance`
- [x] `RegionImage`
- [x] `RegionIntersection`
- [x] `RegionMaxCurvature`
- [x] `RegionMeanCurvature`
- [x] `RegionMeasure`
- [x] `RegionMember`
- [x] `RegionMemberFunction`
- [x] `RegionMinCurvature`
- [x] `RegionMoment`
- [x] `RegionNearest`
- [x] `RegionNearestFunction`
- [x] `RegionProduct`
- [x] `RegionResize`
- [x] `RegionSimilar`
- [x] `RegionSymmetricDifference`
- [x] `RegionUnion`
- [x] `RegionWithin`
- [x] `RepairMesh`
- [x] `RipleyRassonRegion`
- [x] `ShellRegion`
- [x] `SierpinskiMesh`
- [x] `SignedRegionDistance`
- [x] `SimplifyMesh`
- [x] `SmoothMesh`
- [x] `SubdivisionRegion`
- [x] `TransformedRegion`
- [x] `TriangulateMesh`
- [x] `VoronoiMesh`

---

## Color Functions

**Count:** 36  **Status:** Pending

- [x] `CMYKColor`
- [x] `ColorBalance`
- [x] `ColorCombine`
- [x] `ColorConvert`
- [x] `ColorData`
- [x] `ColorDataFunction`
- [x] `ColorDetect`
- [x] `ColorDistance`
- [x] `Colorize`
- [x] `ColorNegate`
- [x] `ColorProfileData`
- [x] `ColorQuantize`
- [x] `ColorReplace`
- [x] `ColorSeparate`
- [x] `ColorSetter`
- [x] `ColorSetterBox`
- [x] `ColorSlider`
- [x] `ColorsNear`
- [x] `ColorToneMapping`
- [x] `DominantColors`
- [x] `FindEdgeColoring`
- [x] `FindMatchingColor`
- [x] `FindPlanarColoring`
- [x] `FindVertexColoring`
- [x] `Hue`
- [x] `ImageColorSpace`
- [x] `LABColor`
- [x] `LCHColor`
- [x] `LUVColor`
- [x] `RandomColor`
- [x] `RGBColor`
- [x] `SurfaceColor`
- [x] `SystemColor`
- [x] `ThemeColor`
- [x] `ToColor`
- [x] `XYZColor`

---

## General Functions (80 batches of ~50)


### Batch 01: AASTriangle to AircraftData

**Status:** Pending

- [x] `AASTriangle`
- [x] `AbelianGroup`
- [x] `Abort`
- [x] `AbortKernels`
- [x] `AbortProtect`
- [x] `AbortScheduledTask`
- [x] `AbsArg`
- [x] `AbsoluteCorrelation`
- [x] `AbsoluteCorrelationFunction`
- [x] `AbsoluteCurrentValue`
- [x] `AbsoluteDashing`
- [x] `AbsoluteOptions`
- [x] `AbsolutePointSize`
- [x] `AbsoluteThickness`
- [x] `AbsoluteTime`
- [x] `AbsoluteTiming`
- [x] `AccountingForm`
- [x] `Accuracy`
- [x] `AcousticAbsorbingValue`
- [x] `AcousticImpedanceValue`
- [x] `AcousticNormalVelocityValue`
- [x] `AcousticPDEComponent`
- [x] `AcousticPressureCondition`
- [x] `AcousticRadiationValue`
- [x] `AcousticSoundHardValue`
- [x] `AcousticSoundSoftCondition`
- [x] `ActionMenu`
- [x] `Activate`
- [x] `ActiveClassification`
- [x] `ActiveClassificationObject`
- [x] `ActivePrediction`
- [x] `ActivePredictionObject`
- [x] `AddSides`
- [x] `AddTo`
- [x] `AddToSearchIndex`
- [x] `AddToVectorDatabase`
- [x] `AddUsers`
- [x] `AdjacencyGraph`
- [x] `Adjugate`
- [x] `AdjustmentBox`
- [x] `AdjustTimeSeriesForecast`
- [x] `AdministrativeDivisionData`
- [x] `AffineHalfSpace`
- [x] `AffineSpace`
- [x] `AffineStateSpaceModel`
- [x] `AffineTransform`
- [x] `AggregatedEntityClass`
- [x] `AggregateRows`
- [x] `AggregationLayer`
- [x] `AircraftData`

### Batch 02: AirportData to Antonyms

**Status:** Pending

- [x] `AirportData`
- [x] `AirPressureData`
- [x] `AirSoundAttenuation`
- [x] `AirTemperatureData`
- [x] `AiryAi`
- [x] `AiryAiPrime`
- [x] `AiryAiZero`
- [x] `AiryBi`
- [x] `AiryBiPrime`
- [x] `AiryBiZero`
- [x] `AllMatch`
- [x] `AllSameBy`
- [x] `AllTrue`
- [x] `Alphabet`
- [x] `AlphabeticOrder`
- [x] `AlphabeticSort`
- [x] `AlphaChannel`
- [x] `AlternatingFactorial`
- [x] `AlternatingGroup`
- [x] `Alternatives`
- [x] `AmbientLight`
- [x] `AnatomyData`
- [x] `AnatomyForm`
- [x] `AnatomyStyling`
- [x] `AndersonDarlingTest`
- [x] `AngerJ`
- [x] `AngleBisector`
- [x] `AngleBracket`
- [x] `AnglePath`
- [x] `AnglePath3D`
- [x] `AngleVector`
- [x] `AngularGauge`
- [x] `Animate`
- [x] `AnimatedImage`
- [x] `AnimationVideo`
- [x] `Animator`
- [x] `Annotate`
- [x] `Annotation`
- [x] `AnnotationDelete`
- [x] `AnnotationKeys`
- [x] `AnnotationValue`
- [x] `Annuity`
- [x] `AnnuityDue`
- [x] `Annulus`
- [x] `AnomalyDetection`
- [x] `AnomalyDetectorFunction`
- [x] `Anticommutator`
- [x] `Antihermitian`
- [x] `Antisymmetric`
- [x] `Antonyms`

### Batch 03: AnyMatch to ArrayPad

**Status:** Pending

- [x] `AnyMatch`
- [x] `AnyOrder`
- [x] `AnySubset`
- [x] `AnyTrue`
- [x] `Apart`
- [x] `ApartSquareFree`
- [x] `APIFunction`
- [x] `AppellF1`
- [x] `AppellF2`
- [x] `AppellF3`
- [x] `AppellF4`
- [x] `Append`
- [x] `AppendLayer`
- [x] `AppendTo`
- [x] `Application`
- [x] `Apply`
- [x] `ApplyReaction`
- [x] `ApplySides`
- [x] `ApplyTo`
- [x] `ArcCosDegrees`
- [x] `ArcCotDegrees`
- [x] `ArcCscDegrees`
- [x] `ArcCsch`
- [x] `ArcCurvature`
- [x] `ARCHProcess`
- [x] `ArcLength`
- [x] `ArcSecDegrees`
- [x] `ArcSech`
- [x] `ArcSinDegrees`
- [x] `ArcSinDistribution`
- [x] `ArcTanDegrees`
- [x] `Area`
- [x] `ArgMax`
- [x] `ArgMin`
- [x] `ArgumentsOptions`
- [x] `ARIMAProcess`
- [x] `ArithmeticGeometricMean`
- [x] `ARMAProcess`
- [x] `Around`
- [x] `AroundReplace`
- [x] `ARProcess`
- [x] `ARPublish`
- [x] `Array`
- [x] `ArrayComponents`
- [x] `ArrayDepth`
- [x] `ArrayDot`
- [x] `ArrayExpand`
- [x] `ArrayFilter`
- [x] `ArrayFlatten`
- [x] `ArrayPad`

### Batch 04: ArrayReduce to AttachCell

**Status:** Pending

- [x] `ArrayReduce`
- [x] `ArrayResample`
- [x] `ArrayReshape`
- [x] `ArrayRules`
- [x] `Arrays`
- [x] `ArraySimplify`
- [x] `ArraySymbol`
- [x] `Arrow`
- [x] `Arrowheads`
- [x] `ASATriangle`
- [x] `Ask`
- [x] `AskAppend`
- [x] `AskConfirm`
- [x] `AskDisplay`
- [x] `AskedValue`
- [x] `AskFunction`
- [x] `AskState`
- [x] `AskTemplateDisplay`
- [x] `Assert`
- [x] `AssessmentFunction`
- [x] `AssessmentResultObject`
- [x] `Assuming`
- [x] `AstroAngularSeparation`
- [x] `AstroDistance`
- [x] `AstroGraphics`
- [x] `AstronomicalData`
- [x] `AstroPosition`
- [x] `AstroRiseSet`
- [x] `AstroStyling`
- [x] `AstroSubpoint`
- [x] `Asymptotic`
- [x] `AsymptoticDSolveValue`
- [x] `AsymptoticEqual`
- [x] `AsymptoticEquivalent`
- [x] `AsymptoticExpectation`
- [x] `AsymptoticGreater`
- [x] `AsymptoticGreaterEqual`
- [x] `AsymptoticIntegrate`
- [x] `AsymptoticLess`
- [x] `AsymptoticLessEqual`
- [x] `AsymptoticOutputTracker`
- [x] `AsymptoticProbability`
- [x] `AsymptoticProduct`
- [x] `AsymptoticRSolveValue`
- [x] `AsymptoticSolve`
- [x] `AsymptoticSum`
- [x] `AsynchronousTaskObject`
- [x] `AsynchronousTasks`
- [x] `Atom`
- [x] `AttachCell`

### Batch 05: AttentionLayer to BernoulliDistribution

**Status:** Pending

- [x] `AttentionLayer`
- [x] `Attributes`
- [x] `AugmentedPolyhedron`
- [x] `AuthenticationDialog`
- [x] `Autocomplete`
- [x] `AutocompletionFunction`
- [x] `AutocorrelationTest`
- [x] `AutoRefreshed`
- [x] `AutoSubmitting`
- [x] `AxiomaticTheory`
- [x] `AxisObject`
- [x] `BabyMonsterGroupB`
- [x] `Backslash`
- [x] `Band`
- [x] `BandpassFilter`
- [x] `BandstopFilter`
- [x] `BarabasiAlbertGraphDistribution`
- [x] `BarcodeImage`
- [x] `BarcodeRecognize`
- [x] `BaringhausHenzeTest`
- [x] `BarLegend`
- [x] `BarlowProschanImportance`
- [x] `BarnesG`
- [x] `BartlettHannWindow`
- [x] `BartlettWindow`
- [x] `BaseDecode`
- [x] `BaseEncode`
- [x] `BaseForm`
- [x] `BasicRecurrentLayer`
- [x] `BatchNormalizationLayer`
- [x] `BatesDistribution`
- [x] `BattleLemarieWavelet`
- [x] `BayesianMaximization`
- [x] `BayesianMaximizationObject`
- [x] `BayesianMinimization`
- [x] `BayesianMinimizationObject`
- [x] `Because`
- [x] `BeckmannDistribution`
- [x] `Beep`
- [x] `Begin`
- [x] `BeginDialogPacket`
- [x] `BeginPackage`
- [x] `BellB`
- [x] `BellY`
- [x] `BenfordDistribution`
- [x] `BeniniDistribution`
- [x] `BenktanderGibratDistribution`
- [x] `BenktanderWeibullDistribution`
- [x] `BernoulliB`
- [x] `BernoulliDistribution`

### Batch 06: BernoulliGraphDistribution to BiquadraticFilterModel

**Status:** Pending

- [x] `BernoulliGraphDistribution`
- [x] `BernoulliProcess`
- [x] `BernsteinBasis`
- [x] `BesagL`
- [x] `BesselFilterModel`
- [x] `BesselI`
- [x] `BesselJ`
- [x] `BesselJZero`
- [x] `BesselK`
- [x] `BesselY`
- [x] `BesselYZero`
- [x] `Beta`
- [x] `BetaBinomialDistribution`
- [x] `BetaDistribution`
- [x] `BetaNegativeBinomialDistribution`
- [x] `BetaPrimeDistribution`
- [x] `BetaRegularized`
- [x] `Between`
- [x] `BetweennessCentrality`
- [x] `BeveledPolyhedron`
- [x] `BezierCurve`
- [x] `BezierFunction`
- [x] `BezierSurface`
- [x] `BilateralFilter`
- [x] `BilateralLaplaceTransform`
- [x] `BilateralZTransform`
- [x] `Binarize`
- [x] `BinaryDeserialize`
- [x] `BinaryDistance`
- [x] `BinaryRead`
- [x] `BinarySerialize`
- [x] `BinaryWrite`
- [x] `BinCounts`
- [x] `BinLists`
- [x] `BinomialDistribution`
- [x] `BinomialPointProcess`
- [x] `BinomialProcess`
- [x] `BinormalDistribution`
- [x] `BioMolecule`
- [x] `BioMoleculeAlign`
- [x] `BioMoleculeValue`
- [x] `BiorthogonalSplineWavelet`
- [x] `BioSequence`
- [x] `BioSequenceComplement`
- [x] `BioSequenceInstances`
- [x] `BioSequenceModify`
- [x] `BioSequenceReverseComplement`
- [x] `BioSequenceTranscribe`
- [x] `BioSequenceTranslate`
- [x] `BiquadraticFilterModel`

### Batch 07: BirnbaumImportance to BorelTannerDistribution

**Status:** Pending

- [x] `BirnbaumImportance`
- [x] `BirnbaumSaundersDistribution`
- [x] `BitClear`
- [x] `BitFlip`
- [x] `BitGet`
- [x] `BitLength`
- [x] `BitSet`
- [x] `BiweightLocation`
- [x] `BiweightMidvariance`
- [x] `BlackmanHarrisWindow`
- [x] `BlackmanNuttallWindow`
- [x] `BlackmanWindow`
- [x] `Blank`
- [x] `BlankNullSequence`
- [x] `BlankSequence`
- [x] `Blend`
- [x] `Block`
- [x] `BlockchainAddressData`
- [x] `BlockchainBlockData`
- [x] `BlockchainContractValue`
- [x] `BlockchainData`
- [x] `BlockchainGet`
- [x] `BlockchainKeyEncode`
- [x] `BlockchainPut`
- [x] `BlockchainTokenData`
- [x] `BlockchainTransaction`
- [x] `BlockchainTransactionData`
- [x] `BlockchainTransactionSign`
- [x] `BlockchainTransactionSubmit`
- [x] `BlockMap`
- [x] `BlockRandom`
- [x] `BlomqvistBeta`
- [x] `BlomqvistBetaTest`
- [x] `Blur`
- [x] `Blurring`
- [x] `BohmanWindow`
- [x] `Bond`
- [x] `Boole`
- [x] `BooleanConsecutiveFunction`
- [x] `BooleanConvert`
- [x] `BooleanCountingFunction`
- [x] `BooleanFunction`
- [x] `BooleanGraph`
- [x] `BooleanMaxterms`
- [x] `BooleanMinimize`
- [x] `BooleanMinterms`
- [x] `BooleanTable`
- [x] `BooleanVariables`
- [x] `BorderDimensions`
- [x] `BorelTannerDistribution`

### Batch 08: BottomHatTransform to CantorStaircase

**Status:** Pending

- [x] `BottomHatTransform`
- [x] `BoundaryDiscretizeGraphics`
- [x] `BoxData`
- [x] `BoxObject`
- [x] `Bra`
- [x] `BracketingBar`
- [x] `BraKet`
- [x] `BrayCurtisDistance`
- [x] `BreadthFirstScan`
- [x] `Break`
- [x] `BridgeData`
- [x] `BrightnessEqualize`
- [x] `BroadcastStationData`
- [x] `BrownForsytheTest`
- [x] `BrownianBridgeProcess`
- [x] `BSplineBasis`
- [x] `BSplineCurve`
- [x] `BSplineFunction`
- [x] `BSplineSurface`
- [x] `BubbleHistogram`
- [x] `BuckyballGraph`
- [x] `BuildCompiledComponent`
- [x] `BuildingData`
- [x] `BulletGauge`
- [x] `BunchKaufmanDecomposition`
- [x] `ButterflyGraph`
- [x] `ButterworthFilterModel`
- [x] `Button`
- [x] `ButtonBar`
- [x] `ButtonBox`
- [x] `ButtonNotebook`
- [x] `ByteArray`
- [x] `ByteArrayFormat`
- [x] `ByteArrayToString`
- [x] `C`
- [x] `CalendarConvert`
- [x] `CalendarData`
- [x] `CalibratedSystemModel`
- [x] `Callout`
- [x] `CallPacket`
- [x] `CanberraDistance`
- [x] `Cancel`
- [x] `CancelButton`
- [x] `CanonicalGraph`
- [x] `CanonicalizePolygon`
- [x] `CanonicalizePolyhedron`
- [x] `CanonicalName`
- [x] `CanonicalWarpingCorrespondence`
- [x] `CanonicalWarpingDistance`
- [x] `CantorStaircase`

### Batch 09: Canvas to CenteredInterval

**Status:** Pending

- [x] `Canvas`
- [x] `Cap`
- [x] `CapForm`
- [x] `CapitalDifferentialD`
- [x] `Capitalize`
- [x] `CapsuleShape`
- [x] `CaputoD`
- [x] `CarlemanLinearize`
- [x] `CarlsonRC`
- [x] `CarlsonRD`
- [x] `CarlsonRE`
- [x] `CarlsonRF`
- [x] `CarlsonRG`
- [x] `CarlsonRJ`
- [x] `CarlsonRK`
- [x] `CarlsonRM`
- [x] `CarmichaelLambda`
- [x] `Cases`
- [x] `CaseSensitive`
- [x] `Cashflow`
- [x] `Casoratian`
- [x] `Cast`
- [x] `CastColumns`
- [x] `Catch`
- [x] `CatchExceptions`
- [x] `CategoricalDistribution`
- [x] `CategoricalHistogram`
- [x] `CategoricalValue`
- [x] `Catenate`
- [x] `CatenateLayer`
- [x] `CauchyDistribution`
- [x] `CauchyPointProcess`
- [x] `CauchyWindow`
- [x] `CayleyGraph`
- [x] `CDF`
- [x] `CDFDeploy`
- [x] `CDFInformation`
- [x] `CDFWavelet`
- [x] `Cell`
- [x] `CellGroup`
- [x] `CellGroupData`
- [x] `CellObject`
- [x] `CellPrint`
- [x] `Cells`
- [x] `CellularAutomaton`
- [x] `CensoredDistribution`
- [x] `Censoring`
- [x] `CenterArray`
- [x] `CenterDot`
- [x] `CenteredInterval`

### Batch 10: CentralFeature to CircleMinus

**Status:** Pending

- [x] `CentralFeature`
- [x] `CentralMoment`
- [x] `CentralMomentGeneratingFunction`
- [x] `Cepstrogram`
- [x] `CepstrogramArray`
- [x] `CepstrumArray`
- [x] `CForm`
- [x] `ChannelListen`
- [x] `ChannelListener`
- [x] `ChannelListeners`
- [x] `ChannelObject`
- [x] `ChannelReceiverFunction`
- [x] `ChannelSend`
- [x] `ChannelSubscribers`
- [x] `ChanVeseBinarize`
- [x] `CharacterCounts`
- [x] `CharacteristicFunction`
- [x] `CharacterName`
- [x] `CharacterNormalize`
- [x] `CharacterRange`
- [x] `Characters`
- [x] `ChatEvaluate`
- [x] `ChatObject`
- [x] `ChatSubmit`
- [x] `Chebyshev1FilterModel`
- [x] `Chebyshev2FilterModel`
- [x] `ChebyshevDistance`
- [x] `ChebyshevT`
- [x] `ChebyshevU`
- [x] `Check`
- [x] `CheckAbort`
- [x] `CheckAll`
- [x] `CheckArguments`
- [x] `Checkbox`
- [x] `CheckboxBar`
- [x] `ChemicalConvert`
- [x] `ChemicalData`
- [x] `ChemicalFormula`
- [x] `ChemicalInstance`
- [x] `ChemicalReaction`
- [x] `ChessboardDistance`
- [x] `ChiDistribution`
- [x] `ChineseRemainder`
- [x] `ChiSquareDistribution`
- [x] `ChoiceButtons`
- [x] `ChoiceDialog`
- [x] `CholeskyDecomposition`
- [x] `Circle`
- [x] `CircleDot`
- [x] `CircleMinus`

### Batch 11: CirclePlus to ClusteringComponents

**Status:** Pending

- [x] `CirclePlus`
- [x] `CirclePoints`
- [x] `CircleThrough`
- [x] `CircleTimes`
- [x] `CirculantGraph`
- [x] `CircularArcThrough`
- [x] `CircumscribedBall`
- [x] `Circumsphere`
- [x] `CityData`
- [x] `Classify`
- [x] `Clear`
- [x] `ClearAll`
- [x] `ClearAttributes`
- [x] `ClearCookies`
- [x] `ClearDistributedDefinitions`
- [x] `ClearPermissions`
- [x] `ClearSystemCache`
- [x] `ClebschGordan`
- [x] `ClickPane`
- [x] `ClickToCopy`
- [x] `CliffordAlgebra`
- [x] `Clock`
- [x] `ClockGauge`
- [x] `Close`
- [x] `CloseKernels`
- [x] `ClosenessCentrality`
- [x] `Closing`
- [x] `CloudAccountData`
- [x] `CloudConnect`
- [x] `CloudDeploy`
- [x] `CloudDisconnect`
- [x] `CloudEvaluate`
- [x] `CloudExport`
- [x] `CloudExpression`
- [x] `CloudExpressions`
- [x] `CloudFunction`
- [x] `CloudGet`
- [x] `CloudImport`
- [x] `CloudLoggingData`
- [x] `CloudObject`
- [x] `CloudObjects`
- [x] `CloudPublish`
- [x] `CloudPut`
- [x] `CloudSave`
- [x] `CloudShare`
- [x] `CloudSubmit`
- [x] `CloudSymbol`
- [x] `CloudUnshare`
- [x] `ClusterClassify`
- [x] `ClusteringComponents`

### Batch 12: ClusteringMeasurements to CompoundPoissonDistribution

**Status:** Pending

- [x] `ClusteringMeasurements`
- [x] `ClusteringTree`
- [x] `Coefficient`
- [x] `CoefficientArrays`
- [x] `CoefficientRules`
- [x] `CoifletWavelet`
- [x] `Collect`
- [x] `CollinearPoints`
- [x] `Colon`
- [x] `ColonForm`
- [x] `Column`
- [x] `ColumnForm`
- [x] `ColumnKeys`
- [x] `ColumnTypes`
- [x] `ColumnwiseCombine`
- [x] `ColumnwiseThread`
- [x] `ColumnwiseValue`
- [x] `Comap`
- [x] `ComapApply`
- [x] `CombinedEntityClass`
- [x] `CometData`
- [x] `Commonest`
- [x] `CommonestFilter`
- [x] `CommonName`
- [x] `CommonUnits`
- [x] `Commutator`
- [x] `CompanyData`
- [x] `Compile`
- [x] `CompiledCodeFunction`
- [x] `CompiledComponent`
- [x] `CompiledComponentRawInterface`
- [x] `CompiledExpressionDeclaration`
- [x] `CompiledFunction`
- [x] `CompiledLayer`
- [x] `CompilerCallback`
- [x] `CompilerEnvironmentAppendTo`
- [x] `CompilerInformation`
- [x] `Complement`
- [x] `ComplementedEntityClass`
- [x] `CompleteGraph`
- [x] `CompleteIntegral`
- [x] `CompleteKaryTree`
- [x] `ComplexExpand`
- [x] `ComponentExpand`
- [x] `ComponentMeasurements`
- [x] `ComposeSeries`
- [x] `Composition`
- [x] `CompoundElement`
- [x] `CompoundExpression`
- [x] `CompoundPoissonDistribution`

### Batch 13: CompoundPoissonProcess to ContourIntegrate

**Status:** Pending

- [x] `CompoundPoissonProcess`
- [x] `CompoundRenewalProcess`
- [x] `Compress`
- [x] `Condition`
- [x] `ConditionalExpression`
- [x] `Conditioned`
- [x] `ConformAudio`
- [x] `ConformDates`
- [x] `ConformImages`
- [x] `Congruent`
- [x] `ConicGradientFilling`
- [x] `ConicOptimization`
- [x] `ConjugateTranspose`
- [x] `Conjunction`
- [x] `ConnectedComponents`
- [x] `ConnectedGraphComponents`
- [x] `ConnectedMoleculeComponents`
- [x] `ConnectLibraryCallbackFunction`
- [x] `ConnectSystemModelComponents`
- [x] `ConnectSystemModelController`
- [x] `ConnesWindow`
- [x] `ConoverTest`
- [x] `ConservativeConvectionPDETerm`
- [x] `ConsoleMessage`
- [x] `ConstantArray`
- [x] `ConstantArrayLayer`
- [x] `ConstantImage`
- [x] `ConstantPlusLayer`
- [x] `ConstantTimesLayer`
- [x] `ConstantVideo`
- [x] `ConstellationData`
- [x] `Construct`
- [x] `ConstructColumns`
- [x] `Containing`
- [x] `ContainsExactly`
- [x] `ContainsOnly`
- [x] `ContentDetectorFunction`
- [x] `ContentObject`
- [x] `Context`
- [x] `Contexts`
- [x] `Continue`
- [x] `ContinuedFraction`
- [x] `ContinuedFractionK`
- [x] `ContinuousMarkovProcess`
- [x] `ContinuousTask`
- [x] `ContinuousWaveletData`
- [x] `ContinuousWaveletTransform`
- [x] `ContourDetect`
- [x] `ContourGraphics`
- [x] `ContourIntegrate`

### Batch 14: ContraharmonicMean to Counts

**Status:** Pending

- [x] `ContraharmonicMean`
- [x] `ContrastiveLossLayer`
- [x] `Control`
- [x] `ControlActive`
- [x] `ControllabilityGramian`
- [x] `ControllableDecomposition`
- [x] `ControllerInformation`
- [x] `ControllerManipulate`
- [x] `ControllerState`
- [x] `ConvectionPDETerm`
- [x] `Convergents`
- [x] `ConvexOptimization`
- [x] `ConvolutionLayer`
- [x] `Convolve`
- [x] `ConwayGroupCo1`
- [x] `ConwayGroupCo2`
- [x] `ConwayGroupCo3`
- [x] `CoordinateBoundingBox`
- [x] `CoordinateBoundingBoxArray`
- [x] `CoordinateBounds`
- [x] `CoordinateBoundsArray`
- [x] `CoordinateChartData`
- [x] `CoordinateTransform`
- [x] `CoordinateTransformData`
- [x] `CoplanarPoints`
- [x] `Coproduct`
- [x] `CopulaDistribution`
- [x] `CopyDatabin`
- [x] `CopyToClipboard`
- [x] `CoreNilpotentDecomposition`
- [x] `CornerFilter`
- [x] `Correlation`
- [x] `CorrelationDistance`
- [x] `CorrelationFunction`
- [x] `CorrelationTest`
- [x] `CosDegrees`
- [x] `CoshIntegral`
- [x] `CosineDistance`
- [x] `CosineWindow`
- [x] `CosIntegral`
- [x] `CotDegrees`
- [x] `CoulombF`
- [x] `CoulombG`
- [x] `CoulombH1`
- [x] `CoulombH2`
- [x] `CountDistinct`
- [x] `CountDistinctBy`
- [x] `CountRoots`
- [x] `CountryData`
- [x] `Counts`

### Batch 15: CountsBy to CurrencyConvert

**Status:** Pending

- [x] `CountsBy`
- [x] `CovarianceFunction`
- [x] `CoxianDistribution`
- [x] `CoxIngersollRossProcess`
- [x] `CoxModel`
- [x] `CoxModelFit`
- [x] `CramerVonMisesTest`
- [x] `CreateArchive`
- [x] `CreateChannel`
- [x] `CreateCloudExpression`
- [x] `CreateCompilerEnvironment`
- [x] `CreateDatabin`
- [x] `CreateDataStructure`
- [x] `CreateDataSystemModel`
- [x] `CreateDialog`
- [x] `CreateDocument`
- [x] `CreateForeignCallback`
- [x] `CreateLicenseEntitlement`
- [x] `CreateManagedLibraryExpression`
- [x] `CreateManagedObject`
- [x] `CreateNotebook`
- [x] `CreatePacletArchive`
- [x] `CreatePalette`
- [x] `CreatePermissionsGroup`
- [x] `CreateScheduledTask`
- [x] `CreateSearchIndex`
- [x] `CreateSemanticSearchIndex`
- [x] `CreateSystemModel`
- [x] `CreateTemporary`
- [x] `CreateTypeInstance`
- [x] `CreateUUID`
- [x] `CreateVectorDatabase`
- [x] `CreateWindow`
- [x] `CriticalityFailureImportance`
- [x] `CriticalitySuccessImportance`
- [x] `CriticalSection`
- [x] `CrossEntropyLossLayer`
- [x] `CrossingDetect`
- [x] `CrossingPolygon`
- [x] `CscDegrees`
- [x] `CTCLossLayer`
- [x] `Cube`
- [x] `CubeRoot`
- [x] `Cuboid`
- [x] `Cumulant`
- [x] `CumulantGeneratingFunction`
- [x] `Cup`
- [x] `CupCap`
- [x] `Curl`
- [x] `CurrencyConvert`

### Batch 16: CurrentCompiledFunctionData to DecisionTreeModel

**Status:** Pending

- [x] `CurrentCompiledFunctionData`
- [x] `CurrentDate`
- [x] `CurrentImage`
- [x] `CurrentNotebookImage`
- [x] `CurrentScreenImage`
- [x] `CurrentValue`
- [x] `Curry`
- [x] `CurryApplied`
- [x] `CurvatureFlowFilter`
- [x] `CycleGraph`
- [x] `Cycles`
- [x] `Cyclic`
- [x] `CyclicGroup`
- [x] `Cyclotomic`
- [x] `Cylinder`
- [x] `CylindricalDecomposition`
- [x] `D`
- [x] `DagumDistribution`
- [x] `DamData`
- [x] `DamerauLevenshteinDistance`
- [x] `Darker`
- [x] `DarkModePane`
- [x] `Dashing`
- [x] `DatabaseConnect`
- [x] `DatabaseDisconnect`
- [x] `DatabaseReference`
- [x] `Databin`
- [x] `DatabinAdd`
- [x] `DatabinRemove`
- [x] `Databins`
- [x] `DatabinSubmit`
- [x] `DatabinUpload`
- [x] `DataConnectionObject`
- [x] `DataDistribution`
- [x] `Dataset`
- [x] `DataStructure`
- [x] `DaubechiesWavelet`
- [x] `DavisDistribution`
- [x] `DawsonF`
- [x] `DayHemisphere`
- [x] `DayName`
- [x] `DayNightTerminator`
- [x] `DayPlus`
- [x] `DayRange`
- [x] `DayRound`
- [x] `DeBruijnGraph`
- [x] `DeBruijnSequence`
- [x] `Decapitalize`
- [x] `DecimalForm`
- [x] `DecisionTreeModel`

### Batch 17: DeclareCompiledComponent to DerivedKey

**Status:** Pending

- [x] `DeclareCompiledComponent`
- [x] `DeclarePackage`
- [x] `Decompose`
- [x] `DeconvolutionLayer`
- [x] `Decrement`
- [x] `Decrypt`
- [x] `DedekindEta`
- [x] `DeepSpaceProbeData`
- [x] `Default`
- [x] `DefaultButton`
- [x] `DefaultValues`
- [x] `Defer`
- [x] `DefineInputStreamMethod`
- [x] `DefineOutputStreamMethod`
- [x] `DefineResourceFunction`
- [x] `Definition`
- [x] `DegreeCentrality`
- [x] `DegreeGraphDistribution`
- [x] `DEigensystem`
- [x] `DEigenvalues`
- [x] `Del`
- [x] `Delayed`
- [x] `Delete`
- [x] `DeleteAdjacentDuplicates`
- [x] `DeleteAnomalies`
- [x] `DeleteBorderComponents`
- [x] `DeleteCases`
- [x] `DeleteChannel`
- [x] `DeleteCloudExpression`
- [x] `DeleteColumns`
- [x] `DeleteDuplicates`
- [x] `DeleteDuplicatesBy`
- [x] `DeleteElements`
- [x] `DeleteMissing`
- [x] `DeleteObject`
- [x] `DeletePermissionsKey`
- [x] `DeleteSearchIndex`
- [x] `DeleteSmallComponents`
- [x] `DeleteStopwords`
- [x] `DelimitedSequence`
- [x] `Dendrogram`
- [x] `Denominator`
- [x] `DensityHistogram`
- [x] `Deploy`
- [x] `Depth`
- [x] `DepthFirstScan`
- [x] `Derivative`
- [x] `DerivativeFilter`
- [x] `DerivativePDETerm`
- [x] `DerivedKey`

### Batch 18: DeviceClose to DirectedEdge

**Status:** Pending

- [x] `DeviceClose`
- [x] `DeviceConfigure`
- [x] `DeviceExecute`
- [x] `DeviceExecuteAsynchronous`
- [x] `DeviceObject`
- [x] `DeviceOpen`
- [x] `DeviceRead`
- [x] `DeviceReadBuffer`
- [x] `DeviceReadLatest`
- [x] `DeviceReadTimeSeries`
- [x] `Devices`
- [x] `DeviceStreams`
- [x] `DeviceWrite`
- [x] `DeviceWriteBuffer`
- [x] `DFixedPoints`
- [x] `DGaussianWavelet`
- [x] `Diagonal`
- [x] `Dialog`
- [x] `DialogInput`
- [x] `DialogNotebook`
- [x] `DialogReturn`
- [x] `Diamond`
- [x] `DiceDissimilarity`
- [x] `DictionaryLookup`
- [x] `Diff`
- [x] `Diff3`
- [x] `DiffApply`
- [x] `DifferenceDelta`
- [x] `DifferenceQuotient`
- [x] `DifferenceRoot`
- [x] `DifferenceRootReduce`
- [x] `DifferentialD`
- [x] `DifferentialRoot`
- [x] `DifferentialRootReduce`
- [x] `DifferentiatorFilter`
- [x] `DiffObject`
- [x] `DiffusionPDETerm`
- [x] `DiggleGatesPointProcess`
- [x] `DiggleGrattonPointProcess`
- [x] `DihedralAngle`
- [x] `DihedralGroup`
- [x] `Dilation`
- [x] `DimensionalCombinations`
- [x] `DimensionReduce`
- [x] `DimensionReducerFunction`
- [x] `DimensionReduction`
- [x] `Dimensions`
- [x] `DiracComb`
- [x] `DiracDelta`
- [x] `DirectedEdge`

### Batch 19: DirectedGraph to DistanceTransform

**Status:** Pending

- [x] `DirectedGraph`
- [x] `DirectedInfinity`
- [x] `DirectionalLight`
- [x] `Directive`
- [x] `DirichletBeta`
- [x] `DirichletCharacter`
- [x] `DirichletCondition`
- [x] `DirichletConvolve`
- [x] `DirichletDistribution`
- [x] `DirichletEta`
- [x] `DirichletL`
- [x] `DirichletLambda`
- [x] `DirichletTransform`
- [x] `DirichletWindow`
- [x] `DisableFormatting`
- [x] `Discard`
- [x] `DiscreteAsymptotic`
- [x] `DiscreteChirpZTransform`
- [x] `DiscreteConvolve`
- [x] `DiscreteDelta`
- [x] `DiscreteHadamardTransform`
- [x] `DiscreteHilbertTransform`
- [x] `DiscreteIndicator`
- [x] `DiscreteInputOutputModel`
- [x] `DiscreteLimit`
- [x] `DiscreteLQEstimatorGains`
- [x] `DiscreteLQRegulatorGains`
- [x] `DiscreteLyapunovSolve`
- [x] `DiscreteMarkovProcess`
- [x] `DiscreteMaxLimit`
- [x] `DiscreteMinLimit`
- [x] `DiscreteRatio`
- [x] `DiscreteRiccatiSolve`
- [x] `DiscreteShift`
- [x] `DiscreteUniformDistribution`
- [x] `DiscreteWaveletData`
- [x] `DiscreteWaveletPacketTransform`
- [x] `DiscreteWaveletTransform`
- [x] `DiscretizeGraphics`
- [x] `Discriminant`
- [x] `Disjunction`
- [x] `Disk`
- [x] `DiskSegment`
- [x] `Dispatch`
- [x] `Display`
- [x] `DisplayEndPacket`
- [x] `DisplayForm`
- [x] `DisplayPacket`
- [x] `DisplayString`
- [x] `DistanceTransform`

### Batch 20: Distribute to DownValues

**Status:** Pending

- [x] `Distribute`
- [x] `Distributed`
- [x] `DistributeDefinitions`
- [x] `DistributionFitTest`
- [x] `DistributionParameterAssumptions`
- [x] `Div`
- [x] `Divide`
- [x] `DivideBy`
- [x] `DivideSides`
- [x] `Divisible`
- [x] `Divisors`
- [x] `DivisorSum`
- [x] `DMSString`
- [x] `Do`
- [x] `DocumentGenerator`
- [x] `DocumentGeneratorInformation`
- [x] `DocumentGenerators`
- [x] `DocumentNotebook`
- [x] `Dodecahedron`
- [x] `DominatorTreeGraph`
- [x] `DotEqual`
- [x] `DotLayer`
- [x] `DotPlusLayer`
- [x] `DoubleBracketingBar`
- [x] `DoubleDownArrow`
- [x] `DoubleLeftArrow`
- [x] `DoubleLeftRightArrow`
- [x] `DoubleLeftTee`
- [x] `DoubleLongLeftArrow`
- [x] `DoubleLongLeftRightArrow`
- [x] `DoubleLongRightArrow`
- [x] `DoubleRightArrow`
- [x] `DoubleRightTee`
- [x] `DoubleUpArrow`
- [x] `DoubleUpDownArrow`
- [x] `DoubleVerticalBar`
- [x] `DownArrow`
- [x] `DownArrowBar`
- [x] `DownArrowUpArrow`
- [x] `DownLeftRightVector`
- [x] `DownLeftTeeVector`
- [x] `DownLeftVector`
- [x] `DownLeftVectorBar`
- [x] `DownRightTeeVector`
- [x] `DownRightVector`
- [x] `DownRightVectorBar`
- [x] `Downsample`
- [x] `DownTee`
- [x] `DownTeeArrow`
- [x] `DownValues`

### Batch 21: DownValuesFunction to EllipticLog

**Status:** Pending

- [x] `DownValuesFunction`
- [x] `DrazinInverse`
- [x] `Drop`
- [x] `DropoutLayer`
- [x] `DropShadowing`
- [x] `DSolve`
- [x] `DSolveChangeVariables`
- [x] `DSolveValue`
- [x] `DStabilityConditions`
- [x] `Dt`
- [x] `DualPlanarGraph`
- [x] `DualPolyhedron`
- [x] `DualSystemsModel`
- [x] `DumpGet`
- [x] `DumpSave`
- [x] `Duration`
- [x] `Dynamic`
- [x] `DynamicBox`
- [x] `DynamicGeoGraphics`
- [x] `DynamicImage`
- [x] `DynamicModule`
- [x] `DynamicModuleBox`
- [x] `DynamicSetting`
- [x] `DynamicWrapper`
- [x] `EarthImpactData`
- [x] `EarthquakeData`
- [x] `EccentricityCentrality`
- [x] `EditDistance`
- [x] `EffectiveInterest`
- [x] `Eigensystem`
- [x] `EigenvalueDecomposition`
- [x] `EigenvectorCentrality`
- [x] `ElectricCurrentDensityValue`
- [x] `ElectricCurrentPDEComponent`
- [x] `ElectricFluxDensityValue`
- [x] `ElectricPotentialCondition`
- [x] `ElectricSymmetryValue`
- [x] `ElectrostaticPDEComponent`
- [x] `Element`
- [x] `ElementData`
- [x] `ElementwiseLayer`
- [x] `Eliminate`
- [x] `Ellipsoid`
- [x] `EllipticE`
- [x] `EllipticExp`
- [x] `EllipticExpPrime`
- [x] `EllipticF`
- [x] `EllipticFilterModel`
- [x] `EllipticK`
- [x] `EllipticLog`

### Batch 22: EllipticPi to Erf

**Status:** Pending

- [x] `EllipticPi`
- [x] `EllipticReducedHalfPeriods`
- [x] `EllipticTheta`
- [x] `EllipticThetaPrime`
- [x] `EmbedCode`
- [x] `EmbeddedHTML`
- [x] `EmbeddedService`
- [x] `EmbeddedSQLEntityClass`
- [x] `EmbeddedSQLExpression`
- [x] `EmbeddingLayer`
- [x] `EmitSound`
- [x] `EmpiricalDistribution`
- [x] `EmptySpaceF`
- [x] `Encode`
- [x] `Encrypt`
- [x] `EncryptedObject`
- [x] `End`
- [x] `EndAdd`
- [x] `EndDialogPacket`
- [x] `EndPackage`
- [x] `EngineeringForm`
- [x] `EnterExpressionPacket`
- [x] `EnterTextPacket`
- [x] `Entity`
- [x] `EntityAugmentColumns`
- [x] `EntityClass`
- [x] `EntityCopies`
- [x] `EntityGroup`
- [x] `EntityInstance`
- [x] `EntityPrefetch`
- [x] `EntityProperties`
- [x] `EntityProperty`
- [x] `EntityPropertyClass`
- [x] `EntityRegister`
- [x] `EntityStore`
- [x] `EntityStores`
- [x] `EntityType`
- [x] `EntityTypeName`
- [x] `EntityUnregister`
- [x] `EntityValue`
- [x] `Entropy`
- [x] `EntropyFilter`
- [x] `Environment`
- [x] `EqualTilde`
- [x] `EqualTo`
- [x] `Equilibrium`
- [x] `EquirippleFilterKernel`
- [x] `Equivalent`
- [x] `EquivalentStrain`
- [x] `Erf`

### Batch 23: Erfc to Exponent

**Status:** Pending

- [x] `Erfc`
- [x] `Erfi`
- [x] `ErlangB`
- [x] `ErlangC`
- [x] `ErlangDistribution`
- [x] `Erosion`
- [x] `ErrorBox`
- [x] `EstimatedBackground`
- [x] `EstimatedDistribution`
- [x] `EstimatedPointNormals`
- [x] `EstimatedPointProcess`
- [x] `EstimatedProcess`
- [x] `EstimatedVariogramModel`
- [x] `EstimatorGains`
- [x] `EstimatorRegulator`
- [x] `EuclideanDistance`
- [x] `EulerAngles`
- [x] `EulerCharacteristic`
- [x] `EulerE`
- [x] `Evaluate`
- [x] `EvaluatePacket`
- [x] `EvaluateScheduledTask`
- [x] `EvaluationBox`
- [x] `EvaluationCell`
- [x] `EvaluationData`
- [x] `EvaluationNotebook`
- [x] `EvaluationObject`
- [x] `EventData`
- [x] `EventHandler`
- [x] `EventSeries`
- [x] `EventSeriesAccumulate`
- [x] `EventSeriesLookup`
- [x] `ExactBlackmanWindow`
- [x] `ExampleData`
- [x] `Except`
- [x] `Exception`
- [x] `ExceptionTypes`
- [x] `Exists`
- [x] `Exit`
- [x] `ExoplanetData`
- [x] `Expand`
- [x] `ExpandAll`
- [x] `ExpandDenominator`
- [x] `ExpandNumerator`
- [x] `Expectation`
- [x] `ExpectedValue`
- [x] `ExpGammaDistribution`
- [x] `ExpIntegralE`
- [x] `ExpIntegralEi`
- [x] `Exponent`

### Batch 24: ExponentialDistribution to FeatureExtract

**Status:** Pending

- [x] `ExponentialDistribution`
- [x] `ExponentialGeneratingFunction`
- [x] `ExponentialModel`
- [x] `ExponentialMovingAverage`
- [x] `ExponentialPowerDistribution`
- [x] `ExpressionCell`
- [x] `ExpressionGraph`
- [x] `ExpressionTree`
- [x] `ExpToTrig`
- [x] `ExtendedEntityClass`
- [x] `ExtendedGCD`
- [x] `ExtendedKey`
- [x] `ExternalBundle`
- [x] `ExternalEvaluate`
- [x] `ExternalEvaluatorObject`
- [x] `ExternalEvaluators`
- [x] `ExternalFunction`
- [x] `ExternalIdentifier`
- [x] `ExternalObject`
- [x] `ExternalOperation`
- [x] `ExternalSessionObject`
- [x] `ExternalSessions`
- [x] `ExternalStorageDownload`
- [x] `ExternalStorageGet`
- [x] `ExternalStorageObject`
- [x] `ExternalStoragePut`
- [x] `ExternalStorageUpload`
- [x] `ExternalValue`
- [x] `Extract`
- [x] `ExtractArchive`
- [x] `ExtractLayer`
- [x] `ExtractPacletArchive`
- [x] `ExtremeValueDistribution`
- [x] `FaceAlign`
- [x] `FaceForm`
- [x] `FaceRecognize`
- [x] `FacialFeatures`
- [x] `Factor`
- [x] `FactorialMoment`
- [x] `FactorialMomentGeneratingFunction`
- [x] `FactorialPower`
- [x] `FactorInteger`
- [x] `FactorSquareFree`
- [x] `FactorTerms`
- [x] `Failure`
- [x] `FailureDistribution`
- [x] `FareySequence`
- [x] `FARIMAProcess`
- [x] `FeatureDistance`
- [x] `FeatureExtract`

### Batch 25: FeatureExtraction to FindHamiltonianPath

**Status:** Pending

- [x] `FeatureExtraction`
- [x] `FeatureExtractorFunction`
- [x] `FeatureNearest`
- [x] `FeedbackLinearize`
- [x] `FetalGrowthData`
- [x] `Fibonorial`
- [x] `FilledCurve`
- [x] `FilledPolarCurve`
- [x] `FilledTorus`
- [x] `FillingTransform`
- [x] `FilteredEntityClass`
- [x] `FilterRules`
- [x] `FinancialBond`
- [x] `FinancialData`
- [x] `FinancialDerivative`
- [x] `FinancialIndicator`
- [x] `Find`
- [x] `FindAnomalies`
- [x] `FindArgMax`
- [x] `FindArgMin`
- [x] `FindAstroEvent`
- [x] `FindChannels`
- [x] `FindClique`
- [x] `FindClusters`
- [x] `FindCookies`
- [x] `FindCurvePath`
- [x] `FindCycle`
- [x] `FindDevices`
- [x] `FindDistribution`
- [x] `FindDistributionParameters`
- [x] `FindDivisions`
- [x] `FindEdgeCover`
- [x] `FindEdgeCut`
- [x] `FindEdgeIndependentPaths`
- [x] `FindEquationalProof`
- [x] `FindEulerianCycle`
- [x] `FindExternalEvaluators`
- [x] `FindFaces`
- [x] `FindFit`
- [x] `FindFormula`
- [x] `FindFundamentalCycles`
- [x] `FindGeneratingFunction`
- [x] `FindGeoLocation`
- [x] `FindGeometricConjectures`
- [x] `FindGeometricTransform`
- [x] `FindGraphCommunities`
- [x] `FindGraphIsomorphism`
- [x] `FindGraphPartition`
- [x] `FindHamiltonianCycle`
- [x] `FindHamiltonianPath`

### Batch 26: FindHiddenMarkovStates to FiniteFieldEmbedding

**Status:** Pending

- [x] `FindHiddenMarkovStates`
- [x] `FindImageShapes`
- [x] `FindImageText`
- [x] `FindIndependentEdgeSet`
- [x] `FindIndependentVertexSet`
- [x] `FindInstance`
- [x] `FindIntegerNullVector`
- [x] `FindIsomers`
- [x] `FindIsomorphicSubgraph`
- [x] `FindKClan`
- [x] `FindKClique`
- [x] `FindKClub`
- [x] `FindKPlex`
- [x] `FindLibrary`
- [x] `FindLinearRecurrence`
- [x] `FindMaximumCut`
- [x] `FindMaximumFlow`
- [x] `FindMaxValue`
- [x] `FindMinimumCostFlow`
- [x] `FindMinimumCut`
- [x] `FindMinValue`
- [x] `FindMoleculeSubstructure`
- [x] `FindPath`
- [x] `FindPeaks`
- [x] `FindPermutation`
- [x] `FindPointProcessParameters`
- [x] `FindPostmanTour`
- [x] `FindProcessParameters`
- [x] `FindRepeat`
- [x] `FindSequenceFunction`
- [x] `FindShortestCurve`
- [x] `FindShortestPath`
- [x] `FindShortestTour`
- [x] `FindSolarEclipse`
- [x] `FindSpanningTree`
- [x] `FindSubgraphIsomorphism`
- [x] `FindSystemModelEquilibrium`
- [x] `FindTextualAnswer`
- [x] `FindThreshold`
- [x] `FindTransientRepeat`
- [x] `FindTreeGameStrategies`
- [x] `FindVertexCover`
- [x] `FindVertexCut`
- [x] `FindVertexIndependentPaths`
- [x] `FinishDynamic`
- [x] `FiniteField`
- [x] `FiniteFieldElement`
- [x] `FiniteFieldElementNorm`
- [x] `FiniteFieldElementTrace`
- [x] `FiniteFieldEmbedding`

### Batch 27: FiniteFieldIndex to FourierCosCoefficient

**Status:** Pending

- [x] `FiniteFieldIndex`
- [x] `FiniteGroupData`
- [x] `First`
- [x] `FirstCase`
- [x] `FirstPassageTimeDistribution`
- [x] `FirstPosition`
- [x] `FischerGroupFi22`
- [x] `FischerGroupFi23`
- [x] `FischerGroupFi24Prime`
- [x] `FisherHypergeometricDistribution`
- [x] `FisherRatioTest`
- [x] `FisherZDistribution`
- [x] `Fit`
- [x] `FittedModel`
- [x] `FixedOrder`
- [x] `FixedPoint`
- [x] `FlatShading`
- [x] `Flatten`
- [x] `FlattenAt`
- [x] `FlattenLayer`
- [x] `FlatTopWindow`
- [x] `FlightData`
- [x] `FlipView`
- [x] `FluidFlowPDEComponent`
- [x] `FluidViscosity`
- [x] `FluidViscousStress`
- [x] `Fold`
- [x] `FoldPair`
- [x] `FoldWhile`
- [x] `For`
- [x] `ForAll`
- [x] `ForAllType`
- [x] `ForeignCallback`
- [x] `ForeignFunction`
- [x] `ForeignFunctionLoad`
- [x] `ForeignPointerLookup`
- [x] `Format`
- [x] `FormatValues`
- [x] `FormBox`
- [x] `FormControl`
- [x] `FormFunction`
- [x] `FormObject`
- [x] `FormPage`
- [x] `FormulaData`
- [x] `FormulaLookup`
- [x] `FormulaModel`
- [x] `FortranForm`
- [x] `Fourier`
- [x] `FourierCoefficient`
- [x] `FourierCosCoefficient`

### Batch 28: FourierCosSeries to FromTabular

**Status:** Pending

- [x] `FourierCosSeries`
- [x] `FourierCosTransform`
- [x] `FourierDCT`
- [x] `FourierDCTFilter`
- [x] `FourierDST`
- [x] `FourierSequenceTransform`
- [x] `FourierSeries`
- [x] `FourierSinCoefficient`
- [x] `FourierSinSeries`
- [x] `FourierSinTransform`
- [x] `FourierTransform`
- [x] `FourierTrigSeries`
- [x] `FoxH`
- [x] `FoxHReduce`
- [x] `FractionalBrownianMotionProcess`
- [x] `FractionalD`
- [x] `FractionalGaussianNoiseProcess`
- [x] `FractionBox`
- [x] `FrameBox`
- [x] `Framed`
- [x] `FrameListVideo`
- [x] `FRatioDistribution`
- [x] `FrechetDistribution`
- [x] `FreeformEvaluate`
- [x] `FrenetSerretSystem`
- [x] `FrequencySamplingFilterKernel`
- [x] `FresnelC`
- [x] `FresnelF`
- [x] `FresnelG`
- [x] `FresnelS`
- [x] `FrobeniusAutomorphism`
- [x] `FrobeniusDecomposition`
- [x] `FrobeniusReduce`
- [x] `FrobeniusSolve`
- [x] `FromAbsoluteTime`
- [x] `FromCharacterCode`
- [x] `FromCoefficientRules`
- [x] `FromContinuedFraction`
- [x] `FromDate`
- [x] `FromDateString`
- [x] `FromDMS`
- [x] `FromEntity`
- [x] `FromFiniteField`
- [x] `FromFiniteFieldIndex`
- [x] `FromJulianDate`
- [x] `FromPolarCoordinates`
- [x] `FromRawPointer`
- [x] `FromRomanNumeral`
- [x] `FromSphericalCoordinates`
- [x] `FromTabular`

### Batch 29: FromUnixTime to Gather

**Status:** Pending

- [x] `FromUnixTime`
- [x] `FrontEndExecute`
- [x] `FrontEndToken`
- [x] `FrontEndTokenExecute`
- [x] `FullAxes`
- [x] `FullDefinition`
- [x] `FullForm`
- [x] `FullGraphics`
- [x] `FullInformationOutputRegulator`
- [x] `FullMoon`
- [x] `FullSimplify`
- [x] `Function`
- [x] `FunctionAnalytic`
- [x] `FunctionBijective`
- [x] `FunctionCompile`
- [x] `FunctionCompileExport`
- [x] `FunctionCompileExportByteArray`
- [x] `FunctionCompileExportLibrary`
- [x] `FunctionCompileExportString`
- [x] `FunctionContinuous`
- [x] `FunctionConvexity`
- [x] `FunctionDeclaration`
- [x] `FunctionDiscontinuities`
- [x] `FunctionDomain`
- [x] `FunctionExpand`
- [x] `FunctionInjective`
- [x] `FunctionInterpolation`
- [x] `FunctionLayer`
- [x] `FunctionMeromorphic`
- [x] `FunctionMonotonicity`
- [x] `FunctionPeriod`
- [x] `FunctionPoles`
- [x] `FunctionRange`
- [x] `FunctionSign`
- [x] `FunctionSingularities`
- [x] `FunctionSurjective`
- [x] `FussellVeselyImportance`
- [x] `GaborFilter`
- [x] `GaborWavelet`
- [x] `GainMargins`
- [x] `GainPhaseMargins`
- [x] `GalaxyData`
- [x] `GalleryView`
- [x] `GameTheoryData`
- [x] `Gamma`
- [x] `GammaDistribution`
- [x] `GammaRegularized`
- [x] `GARCHProcess`
- [x] `GatedRecurrentLayer`
- [x] `Gather`

### Batch 30: GatherBy to GeologicalPeriodData

**Status:** Pending

- [x] `GatherBy`
- [x] `GaussianFilter`
- [x] `GaussianWindow`
- [x] `GegenbauerC`
- [x] `GeneralizedLinearModelFit`
- [x] `GeneralizedPolyLog`
- [x] `GeneralizedPower`
- [x] `GenerateAsymmetricKeyPair`
- [x] `GenerateDerivedKey`
- [x] `GenerateDocument`
- [x] `GenerateHTTPResponse`
- [x] `GenerateLLMToolResponse`
- [x] `GenerateSecuredAuthenticationKey`
- [x] `GenerateSymmetricKey`
- [x] `GeneratingFunction`
- [x] `GenericCylindricalDecomposition`
- [x] `GenomeData`
- [x] `GenomeLookup`
- [x] `GeoAntipode`
- [x] `GeoArea`
- [x] `GeoBoundingBox`
- [x] `GeoBounds`
- [x] `GeoCircle`
- [x] `GeodesicClosing`
- [x] `GeodesicDilation`
- [x] `GeodesicErosion`
- [x] `GeodesicOpening`
- [x] `GeodesicPolyhedron`
- [x] `GeoDestination`
- [x] `GeodesyData`
- [x] `GeoDirection`
- [x] `GeoDisk`
- [x] `GeoDisplacement`
- [x] `GeoElevationData`
- [x] `GeoEntities`
- [x] `GeoGraphics`
- [x] `GeogravityModelData`
- [x] `GeoGridDirectionDifference`
- [x] `GeoGridPosition`
- [x] `GeoGridUnitArea`
- [x] `GeoGridUnitDistance`
- [x] `GeoGridVector`
- [x] `GeoGroup`
- [x] `GeoHemisphere`
- [x] `GeoHemisphereBoundary`
- [x] `GeoHistogram`
- [x] `GeoIdentify`
- [x] `GeoImage`
- [x] `GeoLength`
- [x] `GeologicalPeriodData`

### Batch 31: GeomagneticModelData to GreaterEqualLess

**Status:** Pending

- [x] `GeomagneticModelData`
- [x] `GeoMarker`
- [x] `GeometricAssertion`
- [x] `GeometricBrownianMotionProcess`
- [x] `GeometricDistribution`
- [x] `GeometricMean`
- [x] `GeometricMeanFilter`
- [x] `GeometricOptimization`
- [x] `GeometricScene`
- [x] `GeometricSolveValues`
- [x] `GeometricStep`
- [x] `GeometricTest`
- [x] `GeometricTransformation`
- [x] `GeoNearest`
- [x] `GeoOrientationData`
- [x] `GeoPolygon`
- [x] `GeoPosition`
- [x] `GeoPositionENU`
- [x] `GeoPositionXYZ`
- [x] `GeoProjectionData`
- [x] `GeoReposition`
- [x] `GeoSmoothHistogram`
- [x] `GeoStyling`
- [x] `GeoVariant`
- [x] `GeoVector`
- [x] `GeoVectorENU`
- [x] `GeoVectorXYZ`
- [x] `GestureHandler`
- [x] `Get`
- [x] `GetContext`
- [x] `GetEnvironment`
- [x] `GibbsPointProcess`
- [x] `GlobalClusteringCoefficient`
- [x] `Glow`
- [x] `GompertzMakehamDistribution`
- [x] `GoochShading`
- [x] `GoodmanKruskalGamma`
- [x] `GoodmanKruskalGammaTest`
- [x] `Goto`
- [x] `GouraudShading`
- [x] `GPUArray`
- [x] `Grad`
- [x] `GradientFilter`
- [x] `GradientOrientationFilter`
- [x] `GrammarApply`
- [x] `GrammarRules`
- [x] `GrammarToken`
- [x] `GrassmannAlgebra`
- [x] `GrayLevel`
- [x] `GreaterEqualLess`

### Batch 32: GreaterEqualThan to HarmonicMeanFilter

**Status:** Pending

- [x] `GreaterEqualThan`
- [x] `GreaterFullEqual`
- [x] `GreaterGreater`
- [x] `GreaterLess`
- [x] `GreaterSlantEqual`
- [x] `GreaterThan`
- [x] `GreaterTilde`
- [x] `GreenFunction`
- [x] `Grid`
- [x] `GridBox`
- [x] `GridGraph`
- [x] `GridVideo`
- [x] `GroebnerBasis`
- [x] `GroupBy`
- [x] `GroupCentralizer`
- [x] `GroupElementFromWord`
- [x] `GroupElementPosition`
- [x] `GroupElements`
- [x] `GroupElementToWord`
- [x] `GroupGenerators`
- [x] `Groupings`
- [x] `GroupMultiplicationTable`
- [x] `GroupOrbits`
- [x] `GroupOrder`
- [x] `GroupSetwiseStabilizer`
- [x] `GroupStabilizer`
- [x] `GroupStabilizerChain`
- [x] `GrowCutComponents`
- [x] `Gudermannian`
- [x] `GuidedFilter`
- [x] `GumbelDistribution`
- [x] `HaarWavelet`
- [x] `HalfLine`
- [x] `HalfNormalDistribution`
- [x] `HalfPlane`
- [x] `HalfSpace`
- [x] `HalftoneShading`
- [x] `Haloing`
- [x] `HammingDistance`
- [x] `HammingWindow`
- [x] `HankelH1`
- [x] `HankelH2`
- [x] `HankelTransform`
- [x] `HannPoissonWindow`
- [x] `HannWindow`
- [x] `HaradaNortonGroupHN`
- [x] `HararyGraph`
- [x] `HardcorePointProcess`
- [x] `HarmonicMean`
- [x] `HarmonicMeanFilter`

### Batch 33: HarmonicPolyLog to HistogramDistribution

**Status:** Pending

- [x] `HarmonicPolyLog`
- [x] `Hash`
- [x] `HatchFilling`
- [x] `HatchShading`
- [x] `Haversine`
- [x] `HazardFunction`
- [x] `Head`
- [x] `HeadCompose`
- [x] `HeatFluxValue`
- [x] `HeatInsulationValue`
- [x] `HeatOutflowValue`
- [x] `HeatRadiationValue`
- [x] `HeatSymmetryValue`
- [x] `HeatTemperatureCondition`
- [x] `HeatTransferPDEComponent`
- [x] `HeatTransferValue`
- [x] `HeavisideLambda`
- [x] `HeavisidePi`
- [x] `HeavisideTheta`
- [x] `HeldGroupHe`
- [x] `HelmholtzPDEComponent`
- [x] `HermiteDecomposition`
- [x] `HermiteH`
- [x] `HermiteReduce`
- [x] `Hermitian`
- [x] `HessenbergDecomposition`
- [x] `HeunB`
- [x] `HeunBPrime`
- [x] `HeunC`
- [x] `HeunCPrime`
- [x] `HeunD`
- [x] `HeunDPrime`
- [x] `HeunG`
- [x] `HeunGPrime`
- [x] `HeunT`
- [x] `HeunTPrime`
- [x] `Hexahedron`
- [x] `HiddenMarkovProcess`
- [x] `Highlighted`
- [x] `HighlightGraph`
- [x] `HighlightImage`
- [x] `HighlightVideo`
- [x] `HighpassFilter`
- [x] `HigmanSimsGroupHS`
- [x] `HilbertCurve`
- [x] `HilbertFilter`
- [x] `HilbertTransform`
- [x] `Histogram`
- [x] `Histogram3D`
- [x] `HistogramDistribution`

### Batch 34: HistogramPointDensity to HypothesisTestData

**Status:** Pending

- [x] `HistogramPointDensity`
- [x] `HistogramTransform`
- [x] `HistogramTransformInterpolation`
- [x] `HistoricalPeriodData`
- [x] `HitMissTransform`
- [x] `HITSCentrality`
- [x] `HjorthDistribution`
- [x] `HodgeDual`
- [x] `HoeffdingD`
- [x] `HoeffdingDTest`
- [x] `Hold`
- [x] `HoldComplete`
- [x] `HoldCompleteForm`
- [x] `HolderModel`
- [x] `HoldForm`
- [x] `HoldPattern`
- [x] `HolidayCalendarData`
- [x] `HorizontalGauge`
- [x] `HornerForm`
- [x] `HostLookup`
- [x] `HotellingTSquareDistribution`
- [x] `HoytDistribution`
- [x] `HTMLSave`
- [x] `HTTPErrorResponse`
- [x] `HTTPRedirect`
- [x] `HTTPRequest`
- [x] `HTTPRequestData`
- [x] `HTTPResponse`
- [x] `HumanGrowthData`
- [x] `HumpDownHump`
- [x] `HumpEqual`
- [x] `HurwitzLerchPhi`
- [x] `HurwitzZeta`
- [x] `HyperbolicDistribution`
- [x] `HypercubeGraph`
- [x] `HyperexponentialDistribution`
- [x] `Hyperfactorial`
- [x] `Hypergeometric0F1`
- [x] `Hypergeometric0F1Regularized`
- [x] `Hypergeometric1F1`
- [x] `Hypergeometric1F1Regularized`
- [x] `Hypergeometric2F1`
- [x] `Hypergeometric2F1Regularized`
- [x] `HypergeometricDistribution`
- [x] `HypergeometricPFQRegularized`
- [x] `HypergeometricU`
- [x] `Hyperlink`
- [x] `Hyperplane`
- [x] `HypoexponentialDistribution`
- [x] `HypothesisTestData`

### Batch 35: IconData to InputString

**Status:** Pending

- [x] `IconData`
- [x] `Iconize`
- [x] `Icosahedron`
- [x] `IfCompiled`
- [x] `IgnoringInactive`
- [x] `ImplicitD`
- [x] `ImprovementImportance`
- [x] `In`
- [x] `Inactivate`
- [x] `Inactive`
- [x] `IncidenceGraph`
- [x] `Increment`
- [x] `IncrementalFunction`
- [x] `IncrementalObject`
- [x] `IncrementalReceive`
- [x] `IncrementalYield`
- [x] `IndependenceTest`
- [x] `IndependentPhysicalQuantity`
- [x] `IndependentUnit`
- [x] `IndependentUnitDimension`
- [x] `Indexed`
- [x] `IndexEdgeTaggedGraph`
- [x] `IndexGraph`
- [x] `InertEvaluate`
- [x] `InertExpression`
- [x] `InfiniteLine`
- [x] `InfiniteLineThrough`
- [x] `InfinitePlane`
- [x] `Infix`
- [x] `InflationAdjust`
- [x] `Information`
- [x] `InhomogeneousPoissonPointProcess`
- [x] `InhomogeneousPoissonProcess`
- [x] `InitializationObject`
- [x] `InitializationObjects`
- [x] `InitializationValue`
- [x] `Initialize`
- [x] `InnerPolygon`
- [x] `InnerPolyhedron`
- [x] `Inpaint`
- [x] `Input`
- [x] `InputField`
- [x] `InputForm`
- [x] `InputNamePacket`
- [x] `InputNotebook`
- [x] `InputOutputResponse`
- [x] `InputOutputResponseData`
- [x] `InputPacket`
- [x] `InputStream`
- [x] `InputString`

### Batch 36: InputStringPacket to InverseJacobiCD

**Status:** Pending

- [x] `InputStringPacket`
- [x] `InscribedBall`
- [x] `Insert`
- [x] `InsertColumns`
- [x] `InsertLinebreaks`
- [x] `Inset`
- [x] `Insphere`
- [x] `Install`
- [x] `InstallService`
- [x] `InstanceNormalizationLayer`
- [x] `InString`
- [x] `Integrate`
- [x] `IntegrateChangeVariables`
- [x] `InterfaceSwitched`
- [x] `IntermediateTest`
- [x] `InternallyBalancedDecomposition`
- [x] `InterpolatingFunction`
- [x] `Interpolation`
- [x] `Interpretation`
- [x] `InterpretationBox`
- [x] `Interpreter`
- [x] `InterquartileRange`
- [x] `Interrupt`
- [x] `IntersectedEntityClass`
- [x] `Intersection`
- [x] `Interval`
- [x] `IntervalIntersection`
- [x] `IntervalSlider`
- [x] `IntervalUnion`
- [x] `InverseBetaRegularized`
- [x] `InverseBilateralLaplaceTransform`
- [x] `InverseBilateralZTransform`
- [x] `InverseCDF`
- [x] `InverseChiSquareDistribution`
- [x] `InverseContinuousWaveletTransform`
- [x] `InverseDistanceTransform`
- [x] `InverseErf`
- [x] `InverseErfc`
- [x] `InverseFourier`
- [x] `InverseFourierSequenceTransform`
- [x] `InverseFunction`
- [x] `InverseGammaDistribution`
- [x] `InverseGammaRegularized`
- [x] `InverseGaussianDistribution`
- [x] `InverseGudermannian`
- [x] `InverseHankelTransform`
- [x] `InverseHaversine`
- [x] `InverseHilbertTransform`
- [x] `InverseImagePyramid`
- [x] `InverseJacobiCD`

### Batch 37: InverseJacobiCN to JankoGroupJ2

**Status:** Pending

- [x] `InverseJacobiCN`
- [x] `InverseJacobiCS`
- [x] `InverseJacobiDC`
- [x] `InverseJacobiDN`
- [x] `InverseJacobiDS`
- [x] `InverseJacobiNC`
- [x] `InverseJacobiND`
- [x] `InverseJacobiNS`
- [x] `InverseJacobiSC`
- [x] `InverseJacobiSD`
- [x] `InverseJacobiSN`
- [x] `InverseLaplaceTransform`
- [x] `InverseMellinTransform`
- [x] `InversePermutation`
- [x] `InverseRadon`
- [x] `InverseRadonTransform`
- [x] `InverseSeries`
- [x] `InverseShortTimeFourier`
- [x] `InverseSpectrogram`
- [x] `InverseSurvivalFunction`
- [x] `InverseWaveletTransform`
- [x] `InverseWeierstrassP`
- [x] `InverseZTransform`
- [x] `Invisible`
- [x] `IPAddress`
- [x] `IslandData`
- [x] `IsolatingInterval`
- [x] `IsotopeData`
- [x] `Item`
- [x] `ItoProcess`
- [x] `JaccardDissimilarity`
- [x] `JacobiAmplitude`
- [x] `JacobiCD`
- [x] `JacobiCN`
- [x] `JacobiCS`
- [x] `JacobiDC`
- [x] `JacobiDN`
- [x] `JacobiDS`
- [x] `JacobiEpsilon`
- [x] `JacobiNC`
- [x] `JacobiND`
- [x] `JacobiNS`
- [x] `JacobiP`
- [x] `JacobiSC`
- [x] `JacobiSD`
- [x] `JacobiSN`
- [x] `JacobiZeta`
- [x] `JacobiZN`
- [x] `JankoGroupJ1`
- [x] `JankoGroupJ2`

### Batch 38: JankoGroupJ3 to KroneckerProduct

**Status:** Pending

- [x] `JankoGroupJ3`
- [x] `JankoGroupJ4`
- [x] `JarqueBeraALMTest`
- [x] `JohnsonDistribution`
- [x] `Join`
- [x] `JoinAcross`
- [x] `JoinedCurve`
- [x] `JoinForm`
- [x] `JordanDecomposition`
- [x] `JordanModelDecomposition`
- [x] `JordanReduce`
- [x] `JulianDate`
- [x] `JuliaSetBoettcher`
- [x] `JuliaSetPoints`
- [x] `KaiserBesselWindow`
- [x] `KaiserWindow`
- [x] `KalmanEstimator`
- [x] `KalmanFilter`
- [x] `KarhunenLoeveDecomposition`
- [x] `KaryTree`
- [x] `KatzCentrality`
- [x] `KCoreComponents`
- [x] `KDistribution`
- [x] `KEdgeConnectedComponents`
- [x] `KelvinBei`
- [x] `KelvinBer`
- [x] `KelvinKei`
- [x] `KelvinKer`
- [x] `KendallTau`
- [x] `KendallTauTest`
- [x] `KernelConfiguration`
- [x] `KernelConfigurationEdit`
- [x] `KernelEvaluate`
- [x] `KernelFunction`
- [x] `KernelMixtureDistribution`
- [x] `KernelModelFit`
- [x] `KernelObject`
- [x] `Kernels`
- [x] `Ket`
- [x] `KillProcess`
- [x] `KirchhoffGraph`
- [x] `KleinInvariantJ`
- [x] `KnapsackSolve`
- [x] `KnightTourGraph`
- [x] `KnotData`
- [x] `KochCurve`
- [x] `KolmogorovSmirnovTest`
- [x] `KroneckerDelta`
- [x] `KroneckerModelDecomposition`
- [x] `KroneckerProduct`

### Batch 39: KuiperTest to LeftTee

**Status:** Pending

- [x] `KuiperTest`
- [x] `KumaraswamyDistribution`
- [x] `Kurtosis`
- [x] `KuwaharaFilter`
- [x] `KVertexConnectedComponents`
- [x] `Label`
- [x] `Labeled`
- [x] `LabeledGraphicsBox`
- [x] `LaguerreL`
- [x] `LakeData`
- [x] `LambdaComponents`
- [x] `LameC`
- [x] `LameCPrime`
- [x] `LameEigenvalueA`
- [x] `LameEigenvalueB`
- [x] `LameS`
- [x] `LameSPrime`
- [x] `LaminaData`
- [x] `LanczosWindow`
- [x] `LandauDistribution`
- [x] `LanguageData`
- [x] `LanguageIdentify`
- [x] `LaplaceDistribution`
- [x] `LaplaceTransform`
- [x] `Laplacian`
- [x] `LaplacianFilter`
- [x] `LaplacianGaussianFilter`
- [x] `LaplacianPDETerm`
- [x] `Last`
- [x] `Latitude`
- [x] `LatitudeLongitude`
- [x] `LatticeData`
- [x] `LatticeReduce`
- [x] `LaunchKernels`
- [x] `LDLDecomposition`
- [x] `LeapVariant`
- [x] `LearnDistribution`
- [x] `LearnedDistribution`
- [x] `LeastSquares`
- [x] `LeastSquaresFilterKernel`
- [x] `LeftArrow`
- [x] `LeftArrowBar`
- [x] `LeftArrowRightArrow`
- [x] `LeftDownTeeVector`
- [x] `LeftDownVector`
- [x] `LeftDownVectorBar`
- [x] `LeftJoinAcross`
- [x] `LeftRightArrow`
- [x] `LeftRightVector`
- [x] `LeftTee`

### Batch 40: LeftTeeArrow to LindleyDistribution

**Status:** Pending

- [x] `LeftTeeArrow`
- [x] `LeftTeeVector`
- [x] `LeftTriangle`
- [x] `LeftTriangleBar`
- [x] `LeftTriangleEqual`
- [x] `LeftUpDownVector`
- [x] `LeftUpTeeVector`
- [x] `LeftUpVector`
- [x] `LeftUpVectorBar`
- [x] `LeftVector`
- [x] `LeftVectorBar`
- [x] `Legended`
- [x] `LegendreP`
- [x] `Length`
- [x] `LengthWhile`
- [x] `LerchPhi`
- [x] `LessEqualGreater`
- [x] `LessEqualThan`
- [x] `LessFullEqual`
- [x] `LessGreater`
- [x] `LessLess`
- [x] `LessSlantEqual`
- [x] `LessThan`
- [x] `LessTilde`
- [x] `LetterCounts`
- [x] `Level`
- [x] `LeveneTest`
- [x] `LeviCivitaTensor`
- [x] `LevyDistribution`
- [x] `LexicographicOrder`
- [x] `LexicographicSort`
- [x] `LibraryDataType`
- [x] `LibraryFunction`
- [x] `LibraryFunctionDeclaration`
- [x] `LibraryFunctionError`
- [x] `LibraryFunctionInformation`
- [x] `LibraryFunctionLoad`
- [x] `LibraryFunctionUnload`
- [x] `LibraryLoad`
- [x] `LibraryUnload`
- [x] `LicenseEntitlementObject`
- [x] `LicenseEntitlements`
- [x] `LiftingFilterData`
- [x] `LiftingWaveletTransform`
- [x] `LightDarkSwitched`
- [x] `Lighter`
- [x] `LightModePane`
- [x] `Likelihood`
- [x] `Limit`
- [x] `LindleyDistribution`

### Batch 41: Line to LLMPrompt

**Status:** Pending

- [x] `Line`
- [x] `LinearFractionalOptimization`
- [x] `LinearFractionalTransform`
- [x] `LinearGradientFilling`
- [x] `LinearGradientImage`
- [x] `LinearizingTransformationData`
- [x] `LinearLayer`
- [x] `LinearModel`
- [x] `LinearModelFit`
- [x] `LinearOptimization`
- [x] `LinearProgramming`
- [x] `LinearRecurrence`
- [x] `LinearSolveFunction`
- [x] `LineGraph`
- [x] `LineIntegrate`
- [x] `LineLegend`
- [x] `LinkActivate`
- [x] `LinkClose`
- [x] `LinkConnect`
- [x] `LinkCreate`
- [x] `LinkError`
- [x] `LinkFlush`
- [x] `LinkInterrupt`
- [x] `LinkLaunch`
- [x] `LinkObject`
- [x] `LinkPatterns`
- [x] `LinkRankCentrality`
- [x] `LinkRead`
- [x] `LinkReadHeld`
- [x] `Links`
- [x] `LinkWrite`
- [x] `LinkWriteHeld`
- [x] `LiouvilleLambda`
- [x] `ListAnimate`
- [x] `ListConvolve`
- [x] `ListCorrelate`
- [x] `ListDeconvolve`
- [x] `ListFourierSequenceTransform`
- [x] `ListInterpolation`
- [x] `ListPicker`
- [x] `ListPickerBox`
- [x] `ListPlay`
- [x] `ListZTransform`
- [x] `LiteralType`
- [x] `LLMConfiguration`
- [x] `LLMExampleFunction`
- [x] `LLMFunction`
- [x] `LLMGraph`
- [x] `LLMGraphSubmit`
- [x] `LLMPrompt`

### Batch 42: LLMPromptGenerator to LongestOrderedSequence

**Status:** Pending

- [x] `LLMPromptGenerator`
- [x] `LLMResourceFunction`
- [x] `LLMResourceTool`
- [x] `LLMSynthesize`
- [x] `LLMSynthesizeSubmit`
- [x] `LLMTool`
- [x] `LLMToolRequest`
- [x] `LLMToolResponse`
- [x] `LoadCompiledComponent`
- [x] `LocalAdaptiveBinarize`
- [x] `LocalCache`
- [x] `LocalClusteringCoefficient`
- [x] `LocalEvaluate`
- [x] `LocalModelFit`
- [x] `LocalObject`
- [x] `LocalObjects`
- [x] `LocalResponseNormalizationLayer`
- [x] `LocalSubmit`
- [x] `LocalSymbol`
- [x] `LocalTime`
- [x] `LocalTimeZone`
- [x] `LocationEquivalenceTest`
- [x] `LocationTest`
- [x] `Locator`
- [x] `LocatorPane`
- [x] `LogBarnesG`
- [x] `LogGamma`
- [x] `LogGammaDistribution`
- [x] `LogicalExpand`
- [x] `LogIntegral`
- [x] `LogisticDistribution`
- [x] `LogisticSigmoid`
- [x] `LogitModelFit`
- [x] `LogLikelihood`
- [x] `LogLogisticDistribution`
- [x] `LogModel`
- [x] `LogMultinormalDistribution`
- [x] `LogNormalDistribution`
- [x] `LogRankTest`
- [x] `LogSeriesDistribution`
- [x] `LommelS1`
- [x] `LommelS2`
- [x] `LommelT1`
- [x] `LommelT2`
- [x] `Longest`
- [x] `LongestCommonSequence`
- [x] `LongestCommonSequencePositions`
- [x] `LongestCommonSubsequence`
- [x] `LongestCommonSubsequencePositions`
- [x] `LongestOrderedSequence`

### Batch 43: Longitude to MannWhitneyTest

**Status:** Pending

- [x] `Longitude`
- [x] `LongLeftArrow`
- [x] `LongLeftRightArrow`
- [x] `LongRightArrow`
- [x] `LongShortTermMemoryLayer`
- [x] `Lookup`
- [x] `LowerLeftArrow`
- [x] `LowerRightArrow`
- [x] `LowerTriangularize`
- [x] `LowpassFilter`
- [x] `LQEstimatorGains`
- [x] `LQGRegulator`
- [x] `LQOutputRegulatorGains`
- [x] `LQRegulatorGains`
- [x] `LQRegulatorTrain`
- [x] `LuccioSamiComponents`
- [x] `LUDecomposition`
- [x] `LunarEclipse`
- [x] `LyapunovSolve`
- [x] `LyonsGroupLy`
- [x] `MagneticFieldIntensity`
- [x] `MagneticFluxDensity`
- [x] `MagneticFluxDensityValue`
- [x] `MagneticPDEComponent`
- [x] `MagneticPotentialCondition`
- [x] `MagneticSymmetryValue`
- [x] `MagnetostaticPDEComponent`
- [x] `Magnify`
- [x] `MailExecute`
- [x] `MailFolder`
- [x] `MailItem`
- [x] `MailReceiverFunction`
- [x] `MailSearch`
- [x] `MailServerConnect`
- [x] `MailServerConnection`
- [x] `MainSolve`
- [x] `Majority`
- [x] `MakeBoxes`
- [x] `MakeExpression`
- [x] `ManagedLibraryExpressionID`
- [x] `ManagedObject`
- [x] `MandelbrotSetBoettcher`
- [x] `MandelbrotSetDistance`
- [x] `MangoldtLambda`
- [x] `ManhattanDistance`
- [x] `Manipulate`
- [x] `ManipulateVideo`
- [x] `Manipulator`
- [x] `MannedSpaceMissionData`
- [x] `MannWhitneyTest`

### Batch 44: MantissaExponent to MeanAbsoluteLossLayer

**Status:** Pending

- [x] `MantissaExponent`
- [x] `Map`
- [x] `MapAll`
- [x] `MapApply`
- [x] `MapAt`
- [x] `MapIndexed`
- [x] `MAProcess`
- [x] `MapThread`
- [x] `MarchenkoPasturDistribution`
- [x] `MardiaCombinedTest`
- [x] `MardiaKurtosisTest`
- [x] `MardiaSkewnessTest`
- [x] `MarginalDistribution`
- [x] `MarkovProcessProperties`
- [x] `MassConcentrationCondition`
- [x] `MassFluxValue`
- [x] `MassImpermeableBoundaryValue`
- [x] `MassOutflowValue`
- [x] `MassSymmetryValue`
- [x] `MassTransferValue`
- [x] `MassTransportPDEComponent`
- [x] `MatchingDissimilarity`
- [x] `MaterialShading`
- [x] `MaternPointProcess`
- [x] `MathematicalFunctionData`
- [x] `MathieuC`
- [x] `MathieuCharacteristicA`
- [x] `MathieuCharacteristicB`
- [x] `MathieuCharacteristicExponent`
- [x] `MathieuCPrime`
- [x] `MathieuGroupM11`
- [x] `MathieuGroupM12`
- [x] `MathieuGroupM22`
- [x] `MathieuGroupM23`
- [x] `MathieuGroupM24`
- [x] `MathieuS`
- [x] `MathieuSPrime`
- [x] `MathMLForm`
- [x] `Matrices`
- [x] `MaxDate`
- [x] `MaxDetect`
- [x] `MaxFilter`
- [x] `MaximalBy`
- [x] `MaxLimit`
- [x] `MaxMemoryUsed`
- [x] `MaxStableDistribution`
- [x] `MaxValue`
- [x] `MaxwellDistribution`
- [x] `McLaughlinGroupMcL`
- [x] `MeanAbsoluteLossLayer`

### Batch 45: MeanAround to MinusPlus

**Status:** Pending

- [x] `MeanAround`
- [x] `MeanClusteringCoefficient`
- [x] `MeanDegreeConnectivity`
- [x] `MeanDeviation`
- [x] `MeanFilter`
- [x] `MeanGraphDistance`
- [x] `MeanNeighborDegree`
- [x] `MeanPointDensity`
- [x] `MeanShift`
- [x] `MeanShiftFilter`
- [x] `MeanSquaredLossLayer`
- [x] `MedianDeviation`
- [x] `MedianFilter`
- [x] `MedicalTestData`
- [x] `MeijerG`
- [x] `MeijerGReduce`
- [x] `MeixnerDistribution`
- [x] `MellinConvolve`
- [x] `MellinTransform`
- [x] `MemoryAvailable`
- [x] `MemoryConstrained`
- [x] `MemoryInUse`
- [x] `MenuPacket`
- [x] `MenuView`
- [x] `Merge`
- [x] `MersennePrimeExponent`
- [x] `Message`
- [x] `MessageDialog`
- [x] `MessageName`
- [x] `MessagePacket`
- [x] `Messages`
- [x] `MeteorShowerData`
- [x] `MexicanHatWavelet`
- [x] `MeyerWavelet`
- [x] `MidDate`
- [x] `Midpoint`
- [x] `MinDate`
- [x] `MinDetect`
- [x] `MineralData`
- [x] `MinFilter`
- [x] `MinimalBy`
- [x] `MinimalStateSpaceModel`
- [x] `MinimumTimeIncrement`
- [x] `MinkowskiQuestionMark`
- [x] `MinLimit`
- [x] `MinorPlanetData`
- [x] `Minors`
- [x] `MinStableDistribution`
- [x] `Minus`
- [x] `MinusPlus`

### Batch 46: MinValue to Most

**Status:** Pending

- [x] `MinValue`
- [x] `Missing`
- [x] `MissingFallback`
- [x] `MittagLefflerE`
- [x] `MixedFractionParts`
- [x] `MixedMagnitude`
- [x] `MixedRadix`
- [x] `MixedRadixQuantity`
- [x] `MixedUnit`
- [x] `MixtureDistribution`
- [x] `ModelFit`
- [x] `ModelFitReport`
- [x] `ModelPredictiveController`
- [x] `ModularInverse`
- [x] `ModularLambda`
- [x] `Module`
- [x] `Molecule`
- [x] `MoleculeAlign`
- [x] `MoleculeDraw`
- [x] `MoleculeFeatureDistance`
- [x] `MoleculeFingerprint`
- [x] `MoleculeGraph`
- [x] `MoleculeMaximumCommonSubstructure`
- [x] `MoleculeModify`
- [x] `MoleculeName`
- [x] `MoleculePattern`
- [x] `MoleculeProperty`
- [x] `MoleculeRecognize`
- [x] `MoleculeSubstructure`
- [x] `MoleculeSubstructureCases`
- [x] `MoleculeValue`
- [x] `Moment`
- [x] `MomentConvert`
- [x] `MomentEvaluate`
- [x] `MomentGeneratingFunction`
- [x] `MomentOfInertia`
- [x] `Monitor`
- [x] `MonsterGroupM`
- [x] `MoonPhase`
- [x] `MoonPhaseDate`
- [x] `MoonPosition`
- [x] `MorletWavelet`
- [x] `MorphologicalBinarize`
- [x] `MorphologicalBranchPoints`
- [x] `MorphologicalComponents`
- [x] `MorphologicalGraph`
- [x] `MorphologicalPerimeter`
- [x] `MorphologicalTransform`
- [x] `MortalityData`
- [x] `Most`

### Batch 47: MountainData to Nearest

**Status:** Pending

- [x] `MountainData`
- [x] `MouseAnnotation`
- [x] `MouseAppearance`
- [x] `MouseButtons`
- [x] `Mouseover`
- [x] `MousePosition`
- [x] `MovieData`
- [x] `MovingAverage`
- [x] `MovingMap`
- [x] `MovingMedian`
- [x] `MoyalDistribution`
- [x] `Multicolumn`
- [x] `Multinomial`
- [x] `MultinomialDistribution`
- [x] `MultinormalDistribution`
- [x] `MultiplePolyLog`
- [x] `MultipleZeta`
- [x] `MultiplicativeOrder`
- [x] `MultiplySides`
- [x] `MultivariateHypergeometricDistribution`
- [x] `MultivariatePoissonDistribution`
- [x] `MultivariateTDistribution`
- [x] `MusicChord`
- [x] `MusicDuration`
- [x] `MusicInterval`
- [x] `MusicKeySignature`
- [x] `MusicMeasure`
- [x] `MusicMeasurements`
- [x] `MusicNote`
- [x] `MusicPitch`
- [x] `MusicRest`
- [x] `MusicScale`
- [x] `MusicScore`
- [x] `MusicTimeSignature`
- [x] `MusicTransform`
- [x] `MusicVoice`
- [x] `NakagamiDistribution`
- [x] `Names`
- [x] `NArgMax`
- [x] `NArgMin`
- [x] `NBernoulliB`
- [x] `NBodySimulation`
- [x] `NCache`
- [x] `NCaputoD`
- [x] `NContourIntegrate`
- [x] `NDEigensystem`
- [x] `NDEigenvalues`
- [x] `NDSolve`
- [x] `NDSolveValue`
- [x] `Nearest`

### Batch 48: NearestFunction to NonCommutativeGroebnerBasis

**Status:** Pending

- [x] `NearestFunction`
- [x] `NearestModel`
- [x] `NearestNeighborG`
- [x] `NearestNeighborGraph`
- [x] `NearestTo`
- [x] `NebulaData`
- [x] `NeedlemanWunschSimilarity`
- [x] `Needs`
- [x] `Negative`
- [x] `NegativeBinomialDistribution`
- [x] `NegativelyOrientedPoints`
- [x] `NegativeMultinomialDistribution`
- [x] `NeighborhoodData`
- [x] `NeighborhoodGraph`
- [x] `Nest`
- [x] `NestedGreaterGreater`
- [x] `NestedLessLess`
- [x] `NestGraph`
- [x] `NestTree`
- [x] `NestWhile`
- [x] `NeumannBoundaryUnitNormal`
- [x] `NeumannValue`
- [x] `NevilleThetaC`
- [x] `NevilleThetaD`
- [x] `NevilleThetaN`
- [x] `NevilleThetaS`
- [x] `NewMoon`
- [x] `NExpectation`
- [x] `NextCell`
- [x] `NextDate`
- [x] `NextPrime`
- [x] `NextScheduledTaskTime`
- [x] `NextValue`
- [x] `NeymanScottPointProcess`
- [x] `NFractionalD`
- [x] `NightHemisphere`
- [x] `NIntegrate`
- [x] `NLineIntegrate`
- [x] `NMaxValue`
- [x] `NMinValue`
- [x] `Nominal`
- [x] `NominalScale`
- [x] `NoncentralBetaDistribution`
- [x] `NoncentralChiSquareDistribution`
- [x] `NoncentralFRatioDistribution`
- [x] `NoncentralStudentTDistribution`
- [x] `NonCommutativeAlgebra`
- [x] `NonCommutativeCollect`
- [x] `NonCommutativeExpand`
- [x] `NonCommutativeGroebnerBasis`

### Batch 49: NonCommutativeMultiply to NotGreaterSlantEqual

**Status:** Pending

- [x] `NonCommutativeMultiply`
- [x] `NonCommutativeVariables`
- [x] `NondimensionalizationTransform`
- [x] `NoneMatch`
- [x] `NoneTrue`
- [x] `NonlinearModelFit`
- [x] `NonlinearStateSpaceModel`
- [x] `NonlocalMeansFilter`
- [x] `NonNegative`
- [x] `NonPositive`
- [x] `NorlundB`
- [x] `Normal`
- [x] `NormalDistribution`
- [x] `NormalizationLayer`
- [x] `Normalize`
- [x] `NormalizedSquaredEuclideanDistance`
- [x] `NotCongruent`
- [x] `NotCupCap`
- [x] `NotDoubleVerticalBar`
- [x] `Notebook`
- [x] `NotebookApply`
- [x] `NotebookCellData`
- [x] `NotebookClose`
- [x] `NotebookCreate`
- [x] `NotebookDelete`
- [x] `NotebookEvaluate`
- [x] `NotebookFind`
- [x] `NotebookGet`
- [x] `NotebookImport`
- [x] `NotebookInformation`
- [x] `NotebookLocate`
- [x] `NotebookObject`
- [x] `NotebookOpen`
- [x] `NotebookPrint`
- [x] `NotebookPut`
- [x] `NotebookRead`
- [x] `Notebooks`
- [x] `NotebookSave`
- [x] `NotebookSelection`
- [x] `NotebookTemplate`
- [x] `NotebookWrite`
- [x] `NotElement`
- [x] `NotEqualTilde`
- [x] `NotExists`
- [x] `NotGreater`
- [x] `NotGreaterEqual`
- [x] `NotGreaterFullEqual`
- [x] `NotGreaterGreater`
- [x] `NotGreaterLess`
- [x] `NotGreaterSlantEqual`

### Batch 50: NotGreaterTilde to Numerator

**Status:** Pending

- [x] `NotGreaterTilde`
- [x] `Nothing`
- [x] `NotHumpDownHump`
- [x] `NotHumpEqual`
- [x] `NotLeftTriangle`
- [x] `NotLeftTriangleBar`
- [x] `NotLeftTriangleEqual`
- [x] `NotLess`
- [x] `NotLessEqual`
- [x] `NotLessFullEqual`
- [x] `NotLessGreater`
- [x] `NotLessLess`
- [x] `NotLessSlantEqual`
- [x] `NotLessTilde`
- [x] `NotNestedGreaterGreater`
- [x] `NotNestedLessLess`
- [x] `NotPrecedes`
- [x] `NotPrecedesEqual`
- [x] `NotPrecedesSlantEqual`
- [x] `NotPrecedesTilde`
- [x] `NotReverseElement`
- [x] `NotRightTriangle`
- [x] `NotRightTriangleBar`
- [x] `NotRightTriangleEqual`
- [x] `NotSquareSubset`
- [x] `NotSquareSubsetEqual`
- [x] `NotSquareSuperset`
- [x] `NotSquareSupersetEqual`
- [x] `NotSubset`
- [x] `NotSubsetEqual`
- [x] `NotSucceeds`
- [x] `NotSucceedsEqual`
- [x] `NotSucceedsSlantEqual`
- [x] `NotSucceedsTilde`
- [x] `NotSuperset`
- [x] `NotSupersetEqual`
- [x] `NotTilde`
- [x] `NotTildeEqual`
- [x] `NotTildeFullEqual`
- [x] `NotTildeTilde`
- [x] `NotVerticalBar`
- [x] `NProbability`
- [x] `NProduct`
- [x] `NRoots`
- [x] `NSolveValues`
- [x] `NSum`
- [x] `NSurfaceIntegrate`
- [x] `NuclearExplosionData`
- [x] `NuclearReactorData`
- [x] `Numerator`

### Batch 51: NumeratorDenominator to OuterPolygon

**Status:** Pending

- [x] `NumeratorDenominator`
- [x] `NumericalOrder`
- [x] `NumericalSort`
- [x] `NumericArray`
- [x] `NumericArrayType`
- [x] `NuttallWindow`
- [x] `NValues`
- [x] `O`
- [x] `ObjectTrackingData`
- [x] `ObservabilityGramian`
- [x] `ObservableDecomposition`
- [x] `OceanData`
- [x] `Octahedron`
- [x] `Off`
- [x] `Offset`
- [x] `On`
- [x] `ONanGroupON`
- [x] `Once`
- [x] `Opacity`
- [x] `OpaqueRawPointer`
- [x] `OpenAppend`
- [x] `Opener`
- [x] `OpenerView`
- [x] `Opening`
- [x] `OpenRead`
- [x] `OpenTemporary`
- [x] `OpenWrite`
- [x] `Operate`
- [x] `OperationDeclaration`
- [x] `OperatorApplied`
- [x] `OptimumFlowData`
- [x] `Optional`
- [x] `OptionalElement`
- [x] `Options`
- [x] `OptionsPattern`
- [x] `OptionValue`
- [x] `OrbitalElements`
- [x] `Order`
- [x] `OrderDistribution`
- [x] `OrderedSchurDecomposition`
- [x] `Ordering`
- [x] `OrderingBy`
- [x] `OrderingLayer`
- [x] `OrderlessPatternSequence`
- [x] `Ordinal`
- [x] `OrdinalScale`
- [x] `OrnsteinUhlenbeckProcess`
- [x] `Orthogonalize`
- [x] `Out`
- [x] `OuterPolygon`

### Batch 52: OuterPolyhedron to PaletteNotebook

**Status:** Pending

- [x] `OuterPolyhedron`
- [x] `OutputForm`
- [x] `OutputNamePacket`
- [x] `OutputResponse`
- [x] `OutputStream`
- [x] `OverBar`
- [x] `OverDot`
- [x] `Overflow`
- [x] `OverHat`
- [x] `Overlay`
- [x] `OverlayVideo`
- [x] `Overscript`
- [x] `OverscriptBox`
- [x] `OverTilde`
- [x] `OverVector`
- [x] `OwenT`
- [x] `OwnValues`
- [x] `PackageExported`
- [x] `PackageImport`
- [x] `PackageInitialize`
- [x] `PackageScoped`
- [x] `PacletDataRebuild`
- [x] `PacletDisable`
- [x] `PacletEnable`
- [x] `PacletFind`
- [x] `PacletFindRemote`
- [x] `PacletInstall`
- [x] `PacletInstallSubmit`
- [x] `PacletObject`
- [x] `PacletSiteObject`
- [x] `PacletSiteRegister`
- [x] `PacletSites`
- [x] `PacletSiteUnregister`
- [x] `PacletSiteUpdate`
- [x] `PacletSymbol`
- [x] `PacletUninstall`
- [x] `PaddedForm`
- [x] `PaddingLayer`
- [x] `PadeApproximant`
- [x] `PadLeft`
- [x] `PadRight`
- [x] `PageRankCentrality`
- [x] `PairCorrelationG`
- [x] `PairedHistogram`
- [x] `PairedSmoothHistogram`
- [x] `PairedTTest`
- [x] `PairedZTest`
- [x] `PairwiseDensityHistogram`
- [x] `PairwiseSmoothDensityHistogram`
- [x] `PaletteNotebook`

### Batch 53: Pane to Pattern

**Status:** Pending

- [x] `Pane`
- [x] `Panel`
- [x] `PaneSelector`
- [x] `ParabolicCylinderD`
- [x] `ParallelArray`
- [x] `ParallelCases`
- [x] `ParallelCombine`
- [x] `ParallelDo`
- [x] `Parallelepiped`
- [x] `ParallelEvaluate`
- [x] `Parallelize`
- [x] `ParallelKernels`
- [x] `ParallelMap`
- [x] `ParallelNeeds`
- [x] `Parallelogram`
- [x] `ParallelProduct`
- [x] `ParallelSelect`
- [x] `ParallelSubmit`
- [x] `ParallelSum`
- [x] `ParallelTable`
- [x] `ParallelTry`
- [x] `ParameterMixtureDistribution`
- [x] `ParametricConvexOptimization`
- [x] `ParametricFunction`
- [x] `ParametricNDSolve`
- [x] `ParametricNDSolveValue`
- [x] `ParametricRampLayer`
- [x] `ParentBox`
- [x] `ParentCell`
- [x] `Parenthesize`
- [x] `ParentNotebook`
- [x] `ParetoDistribution`
- [x] `ParetoPickandsDistribution`
- [x] `ParkData`
- [x] `Part`
- [x] `PartialCorrelationFunction`
- [x] `PartialFractionElements`
- [x] `PartialFractions`
- [x] `ParticleAcceleratorData`
- [x] `ParticleData`
- [x] `Partition`
- [x] `PartLayer`
- [x] `PartOfSpeech`
- [x] `ParzenWindow`
- [x] `PascalBinomial`
- [x] `PascalDistribution`
- [x] `Paste`
- [x] `PasteButton`
- [x] `PathGraph`
- [x] `Pattern`

### Batch 54: PatternFilling to PhongShading

**Status:** Pending

- [x] `PatternFilling`
- [x] `PatternReaction`
- [x] `PatternSequence`
- [x] `PatternTest`
- [x] `PaulWavelet`
- [x] `Pause`
- [x] `PDF`
- [x] `PeakDetect`
- [x] `PeanoCurve`
- [x] `PearsonChiSquareTest`
- [x] `PearsonCorrelationTest`
- [x] `PearsonDistribution`
- [x] `PenttinenPointProcess`
- [x] `PercentForm`
- [x] `PerceptronModel`
- [x] `Perimeter`
- [x] `PeriodicBoundaryCondition`
- [x] `PeriodicModel`
- [x] `Periodogram`
- [x] `PeriodogramArray`
- [x] `Permanent`
- [x] `PermissionsGroup`
- [x] `PermissionsGroups`
- [x] `PermissionsKey`
- [x] `PermissionsKeys`
- [x] `PermutationCycles`
- [x] `PermutationGroup`
- [x] `PermutationLength`
- [x] `PermutationMax`
- [x] `PermutationMin`
- [x] `PermutationOrder`
- [x] `PermutationPower`
- [x] `PermutationProduct`
- [x] `PermutationReplace`
- [x] `Permutations`
- [x] `PermutationSupport`
- [x] `Permute`
- [x] `PeronaMalikFilter`
- [x] `PerpendicularBisector`
- [x] `PersistenceLocation`
- [x] `PersistentObject`
- [x] `PersistentObjects`
- [x] `PersistentSymbol`
- [x] `PersistentValue`
- [x] `PersonData`
- [x] `PERTDistribution`
- [x] `PetersenGraph`
- [x] `PfaffianDet`
- [x] `PhaseMargins`
- [x] `PhongShading`

### Batch 55: PhysicalSystemData to PolyGamma

**Status:** Pending

- [x] `PhysicalSystemData`
- [x] `Pick`
- [x] `PIDData`
- [x] `PIDTune`
- [x] `Piecewise`
- [x] `PiecewiseExpand`
- [x] `PillaiTrace`
- [x] `PillaiTraceTest`
- [x] `PingTime`
- [x] `PitchRecognize`
- [x] `PivotFromColumns`
- [x] `PivotTable`
- [x] `PivotToColumns`
- [x] `PixelValue`
- [x] `PixelValuePositions`
- [x] `Placed`
- [x] `Placeholder`
- [x] `PlaceholderLayer`
- [x] `PlanarAngle`
- [x] `PlanarGraph`
- [x] `PlanckRadiationLaw`
- [x] `PlaneCurveData`
- [x] `PlanetaryMoonData`
- [x] `PlanetData`
- [x] `PlantData`
- [x] `Play`
- [x] `PlotGrid`
- [x] `Pluralize`
- [x] `PlusMinus`
- [x] `Pochhammer`
- [x] `Point`
- [x] `PointCountDistribution`
- [x] `PointDensity`
- [x] `PointDensityFunction`
- [x] `PointLegend`
- [x] `PointLight`
- [x] `PointProcessFitTest`
- [x] `PointProcessParameterAssumptions`
- [x] `PointSize`
- [x] `PointStatisticFunction`
- [x] `PoissonConsulDistribution`
- [x] `PoissonDistribution`
- [x] `PoissonPDEComponent`
- [x] `PoissonPointProcess`
- [x] `PoissonProcess`
- [x] `PoissonWindow`
- [x] `PolarCurve`
- [x] `PolarDecomposition`
- [x] `PolyaAeppliDistribution`
- [x] `PolyGamma`

### Batch 56: Polygon to PrimeZetaP

**Status:** Pending

- [x] `Polygon`
- [x] `PolygonAngle`
- [x] `PolygonCoordinates`
- [x] `PolygonDecomposition`
- [x] `Polyhedron`
- [x] `PolyhedronAngle`
- [x] `PolyhedronCoordinates`
- [x] `PolyhedronData`
- [x] `PolyhedronDecomposition`
- [x] `PolyhedronGenus`
- [x] `PolyLog`
- [x] `PoolingLayer`
- [x] `PopovDecomposition`
- [x] `PopupMenu`
- [x] `PopupView`
- [x] `PopupWindow`
- [x] `Position`
- [x] `PositionIndex`
- [x] `PositionLargest`
- [x] `PositionSmallest`
- [x] `Positive`
- [x] `PositivelyOrientedPoints`
- [x] `Postfix`
- [x] `PowerDistribution`
- [x] `PowerExpand`
- [x] `PowerMod`
- [x] `PowerModel`
- [x] `PowerRange`
- [x] `PowerSpectralDensity`
- [x] `PowersRepresentations`
- [x] `Precedence`
- [x] `PrecedenceForm`
- [x] `Precedes`
- [x] `PrecedesEqual`
- [x] `PrecedesSlantEqual`
- [x] `PrecedesTilde`
- [x] `Precision`
- [x] `PreDecrement`
- [x] `Predict`
- [x] `PreemptProtect`
- [x] `Prefix`
- [x] `PreIncrement`
- [x] `Prepend`
- [x] `PrependLayer`
- [x] `PrependTo`
- [x] `PreviousCell`
- [x] `PreviousDate`
- [x] `PriceGraphDistribution`
- [x] `PrimePi`
- [x] `PrimeZetaP`

### Batch 57: PrimitiveRoot to QuantityUnit

**Status:** Pending

- [x] `PrimitiveRoot`
- [x] `PrincipalComponents`
- [x] `Printout3D`
- [x] `Prism`
- [x] `PrivateKey`
- [x] `Probability`
- [x] `ProbabilityDistribution`
- [x] `ProbitModelFit`
- [x] `ProcessConnection`
- [x] `Processes`
- [x] `ProcessInformation`
- [x] `ProcessObject`
- [x] `ProcessParameterAssumptions`
- [x] `ProcessStatus`
- [x] `Product`
- [x] `ProductDistribution`
- [x] `ProductLog`
- [x] `ProgressIndicator`
- [x] `Projection`
- [x] `PromptForm`
- [x] `ProofObject`
- [x] `Property`
- [x] `PropertyValue`
- [x] `Proportion`
- [x] `Proportional`
- [x] `Protect`
- [x] `ProteinData`
- [x] `Pruning`
- [x] `PseudoInverse`
- [x] `PsychrometricPropertyData`
- [x] `PublicKey`
- [x] `PulsarData`
- [x] `Put`
- [x] `PutAppend`
- [x] `Pyramid`
- [x] `QBinomial`
- [x] `QFactorial`
- [x] `QGamma`
- [x] `QnDispersion`
- [x] `QPochhammer`
- [x] `QPolyGamma`
- [x] `QRDecomposition`
- [x] `QuadraticOptimization`
- [x] `Quantile`
- [x] `Quantity`
- [x] `QuantityArray`
- [x] `QuantityDistribution`
- [x] `QuantityForm`
- [x] `QuantityMagnitude`
- [x] `QuantityUnit`

### Batch 58: QuantityVariable to RawMemoryImport

**Status:** Pending

- [x] `QuantityVariable`
- [x] `QuantityVariableCanonicalUnit`
- [x] `QuantityVariableDimensions`
- [x] `QuantityVariableIdentifier`
- [x] `QuantityVariablePhysicalQuantity`
- [x] `QuartileDeviation`
- [x] `Quartiles`
- [x] `QuartileSkewness`
- [x] `Query`
- [x] `QuestionGenerator`
- [x] `QuestionInterface`
- [x] `QuestionObject`
- [x] `QuestionSelector`
- [x] `QueueingNetworkProcess`
- [x] `QueueingProcess`
- [x] `QueueProperties`
- [x] `Quiet`
- [x] `QuietEcho`
- [x] `Quit`
- [x] `QuotientRemainder`
- [x] `RadialGradientFilling`
- [x] `RadialGradientImage`
- [x] `RadialityCentrality`
- [x] `RadicalBox`
- [x] `RadioButton`
- [x] `RadioButtonBar`
- [x] `Radon`
- [x] `RadonTransform`
- [x] `RamanujanTau`
- [x] `RamanujanTauL`
- [x] `RamanujanTauTheta`
- [x] `RamanujanTauZ`
- [x] `Ramp`
- [x] `Range`
- [x] `RangeFilter`
- [x] `RangeSpace`
- [x] `RankDecomposition`
- [x] `RankedMax`
- [x] `RankedMin`
- [x] `RarerProbability`
- [x] `Raster`
- [x] `Raster3D`
- [x] `RasterArray`
- [x] `Rasterize`
- [x] `RawBoxes`
- [x] `RawData`
- [x] `RawMemoryAllocate`
- [x] `RawMemoryExport`
- [x] `RawMemoryFree`
- [x] `RawMemoryImport`

### Batch 59: RawMemoryRead to RemoveChannelSubscribers

**Status:** Pending

- [x] `RawMemoryRead`
- [x] `RawMemoryWrite`
- [x] `RawPointer`
- [x] `RayleighDistribution`
- [x] `ReactionBalance`
- [x] `ReactionPDETerm`
- [x] `Read`
- [x] `ReadByteArray`
- [x] `ReadLine`
- [x] `ReadString`
- [x] `Reap`
- [x] `ReapVideo`
- [x] `Rectangle`
- [x] `RectangularRepeatingElement`
- [x] `RecurrenceFilter`
- [x] `RecurrenceTable`
- [x] `Refine`
- [x] `ReflectionTransform`
- [x] `Refresh`
- [x] `RegisterExceptionType`
- [x] `RegisterExternalEvaluator`
- [x] `RegularExpression`
- [x] `RegularPolygon`
- [x] `ReIm`
- [x] `Reinstall`
- [x] `RelationalDatabase`
- [x] `RelationGraph`
- [x] `ReleaseHold`
- [x] `ReliabilityDistribution`
- [x] `ReliefImage`
- [x] `Remesh`
- [x] `RemoteBatchJobAbort`
- [x] `RemoteBatchJobObject`
- [x] `RemoteBatchJobs`
- [x] `RemoteBatchMapSubmit`
- [x] `RemoteBatchSubmissionEnvironment`
- [x] `RemoteBatchSubmit`
- [x] `RemoteConnect`
- [x] `RemoteConnectionObject`
- [x] `RemoteEvaluate`
- [x] `RemoteKernelObject`
- [x] `RemoteRun`
- [x] `RemoteRunProcess`
- [x] `Remove`
- [x] `RemoveAlphaChannel`
- [x] `RemoveAsynchronousTask`
- [x] `RemoveAudioStream`
- [x] `RemoveBackground`
- [x] `RemoveChannelListener`
- [x] `RemoveChannelSubscribers`

### Batch 60: Removed to ReverseApplied

**Status:** Pending

- [x] `Removed`
- [x] `RemoveDiacritics`
- [x] `RemoveInputStreamMethod`
- [x] `RemoveOutputStreamMethod`
- [x] `RemoveProperty`
- [x] `RemoveScheduledTask`
- [x] `RemoveUsers`
- [x] `RemoveVideoStream`
- [x] `RenameColumns`
- [x] `RenewalProcess`
- [x] `Repeated`
- [x] `RepeatedNull`
- [x] `RepeatedTiming`
- [x] `RepeatingElement`
- [x] `Replace`
- [x] `ReplaceAll`
- [x] `ReplaceAt`
- [x] `ReplaceImageValue`
- [x] `ReplacePart`
- [x] `ReplacePixelValue`
- [x] `ReplaceRepeated`
- [x] `ReplicateLayer`
- [x] `ResamplingAlgorithmData`
- [x] `Rescale`
- [x] `RescalingTransform`
- [x] `ResetScheduledTask`
- [x] `ReshapeLayer`
- [x] `Residue`
- [x] `ResidueSum`
- [x] `ResizeLayer`
- [x] `Resolve`
- [x] `ResourceData`
- [x] `ResourceFunction`
- [x] `ResourceObject`
- [x] `ResourceRegister`
- [x] `ResourceRemove`
- [x] `ResourceSearch`
- [x] `ResourceSubmit`
- [x] `ResourceUpdate`
- [x] `ResponseForm`
- [x] `Rest`
- [x] `Restricted`
- [x] `Resultant`
- [x] `ResumePacket`
- [x] `Return`
- [x] `ReturnExpressionPacket`
- [x] `ReturnPacket`
- [x] `ReturnTextPacket`
- [x] `Reverse`
- [x] `ReverseApplied`

### Batch 61: ReverseBiorthogonalSplineWavelet to RootSum

**Status:** Pending

- [x] `ReverseBiorthogonalSplineWavelet`
- [x] `ReverseElement`
- [x] `ReverseEquilibrium`
- [x] `ReverseGraph`
- [x] `ReverseSort`
- [x] `ReverseSortBy`
- [x] `ReverseUpEquilibrium`
- [x] `RFixedPoints`
- [x] `RiccatiSolve`
- [x] `RiceDistribution`
- [x] `RidgeFilter`
- [x] `RiemannR`
- [x] `RiemannSiegelTheta`
- [x] `RiemannSiegelZ`
- [x] `RiemannXi`
- [x] `Riffle`
- [x] `RightArrow`
- [x] `RightArrowBar`
- [x] `RightArrowLeftArrow`
- [x] `RightComposition`
- [x] `RightCosetRepresentative`
- [x] `RightDownTeeVector`
- [x] `RightDownVector`
- [x] `RightDownVectorBar`
- [x] `RightTee`
- [x] `RightTeeArrow`
- [x] `RightTeeVector`
- [x] `RightTriangle`
- [x] `RightTriangleBar`
- [x] `RightTriangleEqual`
- [x] `RightUpDownVector`
- [x] `RightUpTeeVector`
- [x] `RightUpVector`
- [x] `RightUpVectorBar`
- [x] `RightVector`
- [x] `RightVectorBar`
- [x] `RipleyK`
- [x] `RiskAchievementImportance`
- [x] `RiskReductionImportance`
- [x] `RobustConvexOptimization`
- [x] `RogersTanimotoDissimilarity`
- [x] `RollPitchYawAngles`
- [x] `RomanNumeral`
- [x] `Root`
- [x] `RootApproximant`
- [x] `RootIntervals`
- [x] `RootMeanSquare`
- [x] `RootReduce`
- [x] `Roots`
- [x] `RootSum`

### Batch 62: RootTree to SearchIndexObject

**Status:** Pending

- [x] `RootTree`
- [x] `Rotate`
- [x] `RotateLeft`
- [x] `RotateRight`
- [x] `RotationTransform`
- [x] `Row`
- [x] `RowBox`
- [x] `RowKey`
- [x] `RSolve`
- [x] `RSolveValue`
- [x] `RStabilityConditions`
- [x] `RudinShapiro`
- [x] `RudvalisGroupRu`
- [x] `Rule`
- [x] `RuleDelayed`
- [x] `RulesTree`
- [x] `Run`
- [x] `RunProcess`
- [x] `RunScheduledTask`
- [x] `RunThrough`
- [x] `RussellRaoDissimilarity`
- [x] `SameAs`
- [x] `SampledEntityClass`
- [x] `SampledSoundFunction`
- [x] `SamplerModel`
- [x] `SARIMAProcess`
- [x] `SARMAProcess`
- [x] `SASTriangle`
- [x] `SatelliteData`
- [x] `SatisfiabilityInstances`
- [x] `Save`
- [x] `SawtoothWave`
- [x] `Scale`
- [x] `Scaled`
- [x] `ScalingTransform`
- [x] `Scan`
- [x] `ScheduledTask`
- [x] `ScheduledTaskInformation`
- [x] `ScheduledTaskObject`
- [x] `ScheduledTasks`
- [x] `SchrodingerPDEComponent`
- [x] `SchurDecomposition`
- [x] `ScientificForm`
- [x] `ScorerGi`
- [x] `ScorerGiPrime`
- [x] `ScorerHi`
- [x] `ScorerHiPrime`
- [x] `ScrollVideo`
- [x] `SearchAdjustment`
- [x] `SearchIndexObject`

### Batch 63: SearchIndices to SeriesData

**Status:** Pending

- [x] `SearchIndices`
- [x] `SearchQueryString`
- [x] `SearchResultObject`
- [x] `SecDegrees`
- [x] `SechDistribution`
- [x] `SecondOrderConeOptimization`
- [x] `SecuredAuthenticationKey`
- [x] `SecuredAuthenticationKeys`
- [x] `SecurityCertificate`
- [x] `SeedRandom`
- [x] `Select`
- [x] `SelectComponents`
- [x] `SelectedCells`
- [x] `SelectedNotebook`
- [x] `SelectFirst`
- [x] `SelectionAnimate`
- [x] `SelectionCreateCell`
- [x] `SelectionEvaluate`
- [x] `SelectionEvaluateCreateCell`
- [x] `SelectionMove`
- [x] `SemanticImport`
- [x] `SemanticImportString`
- [x] `SemanticInterpretation`
- [x] `SemanticRanking`
- [x] `SemanticSearch`
- [x] `SemanticSearchIndex`
- [x] `SemanticSearchIndices`
- [x] `SemialgebraicComponentInstances`
- [x] `SemidefiniteOptimization`
- [x] `SendMail`
- [x] `SendMessage`
- [x] `Sequence`
- [x] `SequenceAlignment`
- [x] `SequenceAttentionLayer`
- [x] `SequenceCases`
- [x] `SequenceFold`
- [x] `SequenceForm`
- [x] `SequenceIndicesLayer`
- [x] `SequenceLastLayer`
- [x] `SequenceMostLayer`
- [x] `SequencePosition`
- [x] `SequencePredict`
- [x] `SequenceReplace`
- [x] `SequenceRestLayer`
- [x] `SequenceReverseLayer`
- [x] `SequenceSplit`
- [x] `SequenceType`
- [x] `Series`
- [x] `SeriesCoefficient`
- [x] `SeriesData`

### Batch 64: ServiceConnect to ShortTimeFourierData

**Status:** Pending

- [x] `ServiceConnect`
- [x] `ServiceDeploy`
- [x] `ServiceDeployment`
- [x] `ServiceDisconnect`
- [x] `ServiceExecute`
- [x] `ServiceObject`
- [x] `ServiceObjects`
- [x] `ServiceRequest`
- [x] `ServiceSubmit`
- [x] `SessionSubmit`
- [x] `SessionTime`
- [x] `Set`
- [x] `SetAccuracy`
- [x] `SetAlphaChannel`
- [x] `SetAttributes`
- [x] `SetCookies`
- [x] `SetDelayed`
- [x] `SetEnvironment`
- [x] `SetOptions`
- [x] `SetPermissions`
- [x] `SetPrecision`
- [x] `SetProperty`
- [x] `SetSelectedNotebook`
- [x] `SetSharedFunction`
- [x] `SetSharedVariable`
- [x] `SetStreamPosition`
- [x] `SetSystemModel`
- [x] `SetSystemOptions`
- [x] `Setter`
- [x] `SetterBar`
- [x] `Setting`
- [x] `SetUsers`
- [x] `Shallow`
- [x] `ShannonWavelet`
- [x] `ShapiroWilkTest`
- [x] `Share`
- [x] `Sharpen`
- [x] `ShearingTransform`
- [x] `ShiftedGompertzDistribution`
- [x] `ShiftRegisterSequence`
- [x] `Short`
- [x] `ShortDownArrow`
- [x] `Shortest`
- [x] `ShortestCurveDistance`
- [x] `ShortestMatch`
- [x] `ShortestPathFunction`
- [x] `ShortLeftArrow`
- [x] `ShortRightArrow`
- [x] `ShortTimeFourier`
- [x] `ShortTimeFourierData`

### Batch 65: ShortUpArrow to SocialMediaData

**Status:** Pending

- [x] `ShortUpArrow`
- [x] `Show`
- [x] `SiderealTime`
- [x] `SiegelTheta`
- [x] `SiegelTukeyTest`
- [x] `SierpinskiCurve`
- [x] `Signature`
- [x] `SignedRankTest`
- [x] `SignTest`
- [x] `SimpleGraph`
- [x] `Simplex`
- [x] `Simplify`
- [x] `Sinc`
- [x] `SinDegrees`
- [x] `SinghMaddalaDistribution`
- [x] `SingularValueDecomposition`
- [x] `SingularValues`
- [x] `SinhIntegral`
- [x] `SinIntegral`
- [x] `SixJSymbol`
- [x] `Skeleton`
- [x] `SkeletonTransform`
- [x] `SkellamDistribution`
- [x] `Skewness`
- [x] `SkewNormalDistribution`
- [x] `Skip`
- [x] `SliceDistribution`
- [x] `Slider`
- [x] `Slider2D`
- [x] `SliderBox`
- [x] `SlideShowVideo`
- [x] `SlideView`
- [x] `Slot`
- [x] `SlotSequence`
- [x] `SmallCircle`
- [x] `SmithDecomposition`
- [x] `SmithDelayCompensator`
- [x] `SmithReduce`
- [x] `SmithWatermanSimilarity`
- [x] `SmoothDateHistogram`
- [x] `SmoothDensityHistogram`
- [x] `SmoothHistogram`
- [x] `SmoothHistogram3D`
- [x] `SmoothKernelDistribution`
- [x] `SmoothPointDensity`
- [x] `SnDispersion`
- [x] `Snippet`
- [x] `SnippetsVideo`
- [x] `SnubPolyhedron`
- [x] `SocialMediaData`

### Batch 66: SocketConnect to SpearmanRho

**Status:** Pending

- [x] `SocketConnect`
- [x] `SocketListen`
- [x] `SocketListener`
- [x] `SocketObject`
- [x] `SocketOpen`
- [x] `SocketReadMessage`
- [x] `Sockets`
- [x] `SocketWaitAll`
- [x] `SocketWaitNext`
- [x] `SocketWriteMessage`
- [x] `SoftmaxLayer`
- [x] `SokalSneathDissimilarity`
- [x] `SolarEclipse`
- [x] `SolarSystemFeatureData`
- [x] `SolarTime`
- [x] `SolidAngle`
- [x] `SolidBoundaryLoadValue`
- [x] `SolidData`
- [x] `SolidDisplacementCondition`
- [x] `SolidFixedCondition`
- [x] `SolidMechanicsPDEComponent`
- [x] `SolidMechanicsStrain`
- [x] `SolidMechanicsStress`
- [x] `SolveAlways`
- [x] `SolveValues`
- [x] `Sort`
- [x] `SortBy`
- [x] `SortedEntityClass`
- [x] `Sound`
- [x] `SoundNote`
- [x] `SourcePDETerm`
- [x] `Sow`
- [x] `SowVideo`
- [x] `SpaceCurveData`
- [x] `SpaceForm`
- [x] `Spacer`
- [x] `SparseArray`
- [x] `SpatialBinnedPointData`
- [x] `SpatialEstimate`
- [x] `SpatialEstimatorFunction`
- [x] `SpatialGraphDistribution`
- [x] `SpatialJ`
- [x] `SpatialMedian`
- [x] `SpatialPointData`
- [x] `SpatialPointSelect`
- [x] `SpatialRandomnessTest`
- [x] `SpatialTransformationLayer`
- [x] `Speak`
- [x] `SpearmanRankTest`
- [x] `SpearmanRho`

### Batch 67: SpeciesData to SSSTriangle

**Status:** Pending

- [x] `SpeciesData`
- [x] `SpectralLineData`
- [x] `Spectrogram`
- [x] `SpectrogramArray`
- [x] `Specularity`
- [x] `SpeechCases`
- [x] `SpeechInterpreter`
- [x] `SpeechRecognize`
- [x] `SpeechSynthesize`
- [x] `Sphere`
- [x] `SpherePoints`
- [x] `SphericalAngle`
- [x] `SphericalBesselJ`
- [x] `SphericalBesselY`
- [x] `SphericalDistance`
- [x] `SphericalHankelH1`
- [x] `SphericalHankelH2`
- [x] `SphericalHarmonicY`
- [x] `SphericalShell`
- [x] `SpheroidalEigenvalue`
- [x] `SpheroidalJoiningFactor`
- [x] `SpheroidalPS`
- [x] `SpheroidalPSPrime`
- [x] `SpheroidalQS`
- [x] `SpheroidalQSPrime`
- [x] `SpheroidalRadialFactor`
- [x] `SpheroidalS1`
- [x] `SpheroidalS1Prime`
- [x] `SpheroidalS2`
- [x] `SpheroidalS2Prime`
- [x] `Splice`
- [x] `SplicedDistribution`
- [x] `Split`
- [x] `SplitBy`
- [x] `SpokenString`
- [x] `SpotLight`
- [x] `SqrtBox`
- [x] `Square`
- [x] `SquaredEuclideanDistance`
- [x] `SquareIntersection`
- [x] `SquareRepeatingElement`
- [x] `SquaresR`
- [x] `SquareSubset`
- [x] `SquareSubsetEqual`
- [x] `SquareSuperset`
- [x] `SquareSupersetEqual`
- [x] `SquareUnion`
- [x] `SquareWave`
- [x] `Squiggled`
- [x] `SSSTriangle`

### Batch 68: StableDistribution to StudentTDistribution

**Status:** Pending

- [x] `StableDistribution`
- [x] `Stack`
- [x] `StackBegin`
- [x] `StackComplete`
- [x] `StackInhibit`
- [x] `StadiumShape`
- [x] `StandardAtmosphereData`
- [x] `StandardDeviationFilter`
- [x] `StandardForm`
- [x] `Standardize`
- [x] `StandardOceanData`
- [x] `StandbyDistribution`
- [x] `Star`
- [x] `StarClusterData`
- [x] `StarData`
- [x] `StarGraph`
- [x] `StartAsynchronousTask`
- [x] `StartExternalSession`
- [x] `StartProcess`
- [x] `StartScheduledTask`
- [x] `StartWebSession`
- [x] `StateFeedbackGains`
- [x] `StateOutputEstimator`
- [x] `StateResponse`
- [x] `StateSpaceModel`
- [x] `StateSpaceTransform`
- [x] `StateTransformationLinearize`
- [x] `StationaryDistribution`
- [x] `StationaryWaveletPacketTransform`
- [x] `StationaryWaveletTransform`
- [x] `StatusArea`
- [x] `StatusCentrality`
- [x] `StieltjesGamma`
- [x] `StippleShading`
- [x] `StirlingS1`
- [x] `StirlingS2`
- [x] `StopAsynchronousTask`
- [x] `StoppingPowerData`
- [x] `StopScheduledTask`
- [x] `StratonovichProcess`
- [x] `StraussHardcorePointProcess`
- [x] `StraussPointProcess`
- [x] `StreamPosition`
- [x] `Streams`
- [x] `StripBoxes`
- [x] `StructuralImportance`
- [x] `StructuredArray`
- [x] `StruveH`
- [x] `StruveL`
- [x] `StudentTDistribution`

### Batch 69: Style to SuperStar

**Status:** Pending

- [x] `Style`
- [x] `StyleBox`
- [x] `StyleData`
- [x] `StyleForm`
- [x] `StylePrint`
- [x] `Subdivide`
- [x] `Subfactorial`
- [x] `Subgraph`
- [x] `SubMinus`
- [x] `SubPlus`
- [x] `Subresultants`
- [x] `Subscript`
- [x] `SubscriptBox`
- [x] `Subsequences`
- [x] `Subset`
- [x] `SubsetCases`
- [x] `SubsetEqual`
- [x] `SubsetMap`
- [x] `SubsetPosition`
- [x] `SubsetReplace`
- [x] `Subsets`
- [x] `SubStar`
- [x] `SubstitutionSystem`
- [x] `Subsuperscript`
- [x] `SubsuperscriptBox`
- [x] `Subtract`
- [x] `SubtractFrom`
- [x] `SubtractSides`
- [x] `SubValues`
- [x] `Succeeds`
- [x] `SucceedsEqual`
- [x] `SucceedsSlantEqual`
- [x] `SucceedsTilde`
- [x] `Success`
- [x] `SuchThat`
- [x] `Sum`
- [x] `SumConvergence`
- [x] `SummationLayer`
- [x] `SunPosition`
- [x] `Sunrise`
- [x] `Sunset`
- [x] `SuperDagger`
- [x] `SuperMinus`
- [x] `SupernovaData`
- [x] `SuperPlus`
- [x] `Superscript`
- [x] `SuperscriptBox`
- [x] `Superset`
- [x] `SupersetEqual`
- [x] `SuperStar`

### Batch 70: Surd to SystemModelLinearize

**Status:** Pending

- [x] `Surd`
- [x] `SurfaceArea`
- [x] `SurfaceData`
- [x] `SurfaceIntegrate`
- [x] `SurvivalDistribution`
- [x] `SurvivalFunction`
- [x] `SurvivalModel`
- [x] `SurvivalModelFit`
- [x] `SuspendPacket`
- [x] `SuzukiDistribution`
- [x] `SuzukiGroupSuz`
- [x] `SwatchLegend`
- [x] `Symbol`
- [x] `SymbolicDeltaProductArray`
- [x] `SymbolicIdentityArray`
- [x] `SymbolicOnesArray`
- [x] `SymbolicZerosArray`
- [x] `SymbolName`
- [x] `SymletWavelet`
- [x] `Symmetric`
- [x] `SymmetricDifference`
- [x] `SymmetricGroup`
- [x] `SymmetricKey`
- [x] `SymmetricReduction`
- [x] `Symmetrize`
- [x] `SymmetrizedArray`
- [x] `SymmetrizedArrayRules`
- [x] `SymmetrizedDependentComponents`
- [x] `SymmetrizedIndependentComponents`
- [x] `SymmetrizedReplacePart`
- [x] `Synonyms`
- [x] `SyntaxInformation`
- [x] `SyntaxLength`
- [x] `SyntaxPacket`
- [x] `SynthesizeMissingValues`
- [x] `SystemCredential`
- [x] `SystemCredentialData`
- [x] `SystemCredentialKeys`
- [x] `SystemCredentialStoreObject`
- [x] `SystemDialogInput`
- [x] `SystemInformation`
- [x] `SystemInstall`
- [x] `SystemModel`
- [x] `SystemModelAlways`
- [x] `SystemModelCalibrate`
- [x] `SystemModelDelay`
- [x] `SystemModeler`
- [x] `SystemModelEventually`
- [x] `SystemModelExamples`
- [x] `SystemModelLinearize`

### Batch 71: SystemModelMeasurements to TakeLargest

**Status:** Pending

- [x] `SystemModelMeasurements`
- [x] `SystemModelParametricSimulate`
- [x] `SystemModelReliability`
- [x] `SystemModels`
- [x] `SystemModelSimulate`
- [x] `SystemModelSimulateSensitivity`
- [x] `SystemModelSimulationData`
- [x] `SystemModelSurrogate`
- [x] `SystemModelSurrogateTrain`
- [x] `SystemModelSustain`
- [x] `SystemModelUntil`
- [x] `SystemModelValidate`
- [x] `SystemModelValidationData`
- [x] `SystemOpen`
- [x] `SystemOptions`
- [x] `SystemProcessData`
- [x] `SystemProcesses`
- [x] `SystemsConnectionsModel`
- [x] `SystemsModelControllerData`
- [x] `SystemsModelDelay`
- [x] `SystemsModelDelayApproximate`
- [x] `SystemsModelDelete`
- [x] `SystemsModelDimensions`
- [x] `SystemsModelExtract`
- [x] `SystemsModelFeedbackConnect`
- [x] `SystemsModelLinearity`
- [x] `SystemsModelMerge`
- [x] `SystemsModelOrder`
- [x] `SystemsModelParallelConnect`
- [x] `SystemsModelSeriesConnect`
- [x] `SystemsModelStateFeedbackConnect`
- [x] `SystemsModelVectorRelativeOrders`
- [x] `Table`
- [x] `TableForm`
- [x] `TableView`
- [x] `Tabular`
- [x] `TabularColumn`
- [x] `TabularRow`
- [x] `TabularSchema`
- [x] `TabularStructure`
- [x] `TabularSummary`
- [x] `TabView`
- [x] `TagBox`
- [x] `TaggedNestGraph`
- [x] `TagSet`
- [x] `TagSetDelayed`
- [x] `TagUnset`
- [x] `Take`
- [x] `TakeDrop`
- [x] `TakeLargest`

### Batch 72: TakeLargestBy to TextGrid

**Status:** Pending

- [x] `TakeLargestBy`
- [x] `TakeSmallest`
- [x] `TakeSmallestBy`
- [x] `TakeWhile`
- [x] `Tally`
- [x] `TanDegrees`
- [x] `TaskAbort`
- [x] `TaskExecute`
- [x] `TaskObject`
- [x] `TaskRemove`
- [x] `TaskResume`
- [x] `Tasks`
- [x] `TaskSuspend`
- [x] `TaskWait`
- [x] `TelegraphProcess`
- [x] `TemplateApply`
- [x] `TemplateBox`
- [x] `TemplateExpression`
- [x] `TemplateIf`
- [x] `TemplateObject`
- [x] `TemplateSequence`
- [x] `TemplateSlot`
- [x] `TemplateWith`
- [x] `TemporalData`
- [x] `TensorContract`
- [x] `TensorDimensions`
- [x] `TensorExpand`
- [x] `TensorProduct`
- [x] `TensorRank`
- [x] `TensorReduce`
- [x] `TensorSymmetry`
- [x] `TensorTranspose`
- [x] `TensorWedge`
- [x] `TerminatedEvaluation`
- [x] `TestCreate`
- [x] `TestEvaluate`
- [x] `TestObject`
- [x] `TestReport`
- [x] `TestReportObject`
- [x] `TestResultObject`
- [x] `Tetrahedron`
- [x] `TeXForm`
- [x] `TeXSave`
- [x] `Text`
- [x] `TextCases`
- [x] `TextCell`
- [x] `TextContents`
- [x] `TextData`
- [x] `TextElement`
- [x] `TextGrid`

### Batch 73: TextPacket to ToLowerCase

**Status:** Pending

- [x] `TextPacket`
- [x] `TextPosition`
- [x] `TextRecognize`
- [x] `TextSearch`
- [x] `TextSearchReport`
- [x] `TextSentences`
- [x] `TextString`
- [x] `TextStructure`
- [x] `TextSummarize`
- [x] `TextTranslation`
- [x] `Texture`
- [x] `TextWords`
- [x] `Therefore`
- [x] `ThermodynamicData`
- [x] `ThermometerGauge`
- [x] `Thickness`
- [x] `Thinning`
- [x] `ThomasPointProcess`
- [x] `ThompsonGroupTh`
- [x] `Thread`
- [x] `Threaded`
- [x] `ThreadingLayer`
- [x] `ThreeJSymbol`
- [x] `Threshold`
- [x] `Through`
- [x] `Throw`
- [x] `ThrowException`
- [x] `ThueMorse`
- [x] `Thumbnail`
- [x] `TideData`
- [x] `Tilde`
- [x] `TildeEqual`
- [x] `TildeFullEqual`
- [x] `TildeTilde`
- [x] `Timing`
- [x] `TitsGroupT`
- [x] `ToBoxes`
- [x] `ToCharacterCode`
- [x] `ToContinuousTimeModel`
- [x] `ToDate`
- [x] `ToDiscreteTimeModel`
- [x] `ToEntity`
- [x] `ToExpression`
- [x] `ToFiniteField`
- [x] `Together`
- [x] `Toggler`
- [x] `TogglerBar`
- [x] `TogglerBox`
- [x] `ToInvertibleTimeSeries`
- [x] `ToLowerCase`

### Batch 74: ToMemory to TransposeLayer

**Status:** Pending

- [x] `ToMemory`
- [x] `Tooltip`
- [x] `ToonShading`
- [x] `TopHatTransform`
- [x] `ToPolarCoordinates`
- [x] `TopologicalSort`
- [x] `ToRadicals`
- [x] `ToRawPointer`
- [x] `ToRules`
- [x] `Torus`
- [x] `TorusGraph`
- [x] `ToSphericalCoordinates`
- [x] `ToString`
- [x] `ToTabular`
- [x] `TotalLayer`
- [x] `TotalVariationFilter`
- [x] `TouchPosition`
- [x] `ToUpperCase`
- [x] `Tour3DVideo`
- [x] `TourVideo`
- [x] `Trace`
- [x] `TraceDialog`
- [x] `TraceLevel`
- [x] `TracePrint`
- [x] `TraceScan`
- [x] `TracyWidomDistribution`
- [x] `TraditionalForm`
- [x] `TrainImageContentDetector`
- [x] `TrainTextContentDetector`
- [x] `TransferFunctionCancel`
- [x] `TransferFunctionExpand`
- [x] `TransferFunctionFactor`
- [x] `TransferFunctionModel`
- [x] `TransferFunctionPoles`
- [x] `TransferFunctionTransform`
- [x] `TransferFunctionZeros`
- [x] `TransformAnomalies`
- [x] `TransformationFunction`
- [x] `TransformColumns`
- [x] `TransformedDistribution`
- [x] `TransformedField`
- [x] `TransformedProcess`
- [x] `TransformMissing`
- [x] `TransitiveClosureGraph`
- [x] `TransitiveReductionGraph`
- [x] `Translate`
- [x] `TranslationTransform`
- [x] `Transliterate`
- [x] `Transpose`
- [x] `TransposeLayer`

### Batch 75: TravelDirections to Tube

**Status:** Pending

- [x] `TravelDirections`
- [x] `TravelDirectionsData`
- [x] `TravelDistance`
- [x] `TravelTime`
- [x] `Tree`
- [x] `TreeCases`
- [x] `TreeChildren`
- [x] `TreeData`
- [x] `TreeDelete`
- [x] `TreeDepth`
- [x] `TreeExpression`
- [x] `TreeExtract`
- [x] `TreeFold`
- [x] `TreeForm`
- [x] `TreeGame`
- [x] `TreeGamePayoff`
- [x] `TreeGraph`
- [x] `TreeInsert`
- [x] `TreeLeaves`
- [x] `TreeLevel`
- [x] `TreeMap`
- [x] `TreeMapAt`
- [x] `TreeOutline`
- [x] `TreePosition`
- [x] `TreeReplacePart`
- [x] `TreeRules`
- [x] `TreeScan`
- [x] `TreeSelect`
- [x] `TreeSize`
- [x] `Triangle`
- [x] `TriangleCenter`
- [x] `TriangleConstruct`
- [x] `TriangleMeasurement`
- [x] `TriangleWave`
- [x] `TriangularDistribution`
- [x] `TrigExpand`
- [x] `TrigFactor`
- [x] `Trigger`
- [x] `TrigReduce`
- [x] `TrigToExp`
- [x] `TrimmedMean`
- [x] `TrimmedVariance`
- [x] `TropicalStormData`
- [x] `TruncatedDistribution`
- [x] `TruncatedPolyhedron`
- [x] `TruncateSum`
- [x] `TsallisQExponentialDistribution`
- [x] `TsallisQGaussianDistribution`
- [x] `TTest`
- [x] `Tube`

### Batch 76: TukeyLambdaDistribution to UnixTime

**Status:** Pending

- [x] `TukeyLambdaDistribution`
- [x] `TukeyWindow`
- [x] `TunnelData`
- [x] `Tuples`
- [x] `TuranGraph`
- [x] `TuringMachine`
- [x] `TwoWayRule`
- [x] `Typed`
- [x] `TypeDeclaration`
- [x] `TypeEvaluate`
- [x] `TypeHint`
- [x] `TypeOf`
- [x] `TypeSpecifier`
- [x] `Uncompress`
- [x] `UnderBar`
- [x] `Underflow`
- [x] `Underoverscript`
- [x] `UnderoverscriptBox`
- [x] `Underscript`
- [x] `UnderscriptBox`
- [x] `UnderseaFeatureData`
- [x] `UndirectedEdge`
- [x] `UndirectedGraph`
- [x] `UnequalTo`
- [x] `Unevaluated`
- [x] `UniformDistribution`
- [x] `UniformGraphDistribution`
- [x] `UniformPolyhedron`
- [x] `UniformSumDistribution`
- [x] `UnilateralConvolve`
- [x] `UnilateralDiscreteConvolve`
- [x] `Uninstall`
- [x] `Union`
- [x] `UnionedEntityClass`
- [x] `UnionPlus`
- [x] `Unique`
- [x] `UniqueElements`
- [x] `UnitBox`
- [x] `UnitConvert`
- [x] `UnitDimensions`
- [x] `Unitize`
- [x] `UnitRootTest`
- [x] `UnitSimplify`
- [x] `UnitStep`
- [x] `UnitTriangle`
- [x] `UnitVector`
- [x] `UnitVectorLayer`
- [x] `UniverseModelData`
- [x] `UniversityData`
- [x] `UnixTime`

### Batch 77: UnlabeledTree to VarianceEquivalenceTest

**Status:** Pending

- [x] `UnlabeledTree`
- [x] `UnmanageObject`
- [x] `Unprotect`
- [x] `UnregisterExternalEvaluator`
- [x] `Unset`
- [x] `UnsetShared`
- [x] `Until`
- [x] `UpArrow`
- [x] `UpArrowBar`
- [x] `UpArrowDownArrow`
- [x] `Update`
- [x] `UpdateSearchIndex`
- [x] `UpdateSemanticSearchIndex`
- [x] `UpDownArrow`
- [x] `UpEquilibrium`
- [x] `UpperLeftArrow`
- [x] `UpperRightArrow`
- [x] `UpperTriangularize`
- [x] `Upsample`
- [x] `UpSet`
- [x] `UpSetDelayed`
- [x] `UpTee`
- [x] `UpTeeArrow`
- [x] `UpTo`
- [x] `UpValues`
- [x] `URL`
- [x] `URLBuild`
- [x] `URLDecode`
- [x] `URLDispatcher`
- [x] `URLDownload`
- [x] `URLDownloadSubmit`
- [x] `URLEncode`
- [x] `URLExecute`
- [x] `URLExpand`
- [x] `URLFetch`
- [x] `URLFetchAsynchronous`
- [x] `URLParse`
- [x] `URLQueryDecode`
- [x] `URLQueryEncode`
- [x] `URLRead`
- [x] `URLResponseTime`
- [x] `URLSave`
- [x] `URLSaveAsynchronous`
- [x] `URLShorten`
- [x] `URLSubmit`
- [x] `UsingFrontEnd`
- [x] `V2Get`
- [x] `Values`
- [x] `Variables`
- [x] `VarianceEquivalenceTest`

### Batch 78: VarianceGammaDistribution to WavePDEComponent

**Status:** Pending

- [x] `VarianceGammaDistribution`
- [x] `VarianceGammaPointProcess`
- [x] `VarianceTest`
- [x] `VariogramModel`
- [x] `VectorAngle`
- [x] `VectorAround`
- [x] `VectorDatabaseObject`
- [x] `VectorDatabaseObjects`
- [x] `VectorDatabaseSearch`
- [x] `VectorGreater`
- [x] `VectorGreaterEqual`
- [x] `VectorLess`
- [x] `VectorLessEqual`
- [x] `Vectors`
- [x] `VectorSymbol`
- [x] `Vee`
- [x] `Verbatim`
- [x] `VerificationTest`
- [x] `VerifyDerivedKey`
- [x] `VerifyTreeGameStrategy`
- [x] `VerticalBar`
- [x] `VerticalGauge`
- [x] `VerticalSeparator`
- [x] `VerticalSlider`
- [x] `VerticalTilde`
- [x] `VoiceStyleData`
- [x] `VoigtDistribution`
- [x] `VolcanoData`
- [x] `Volume`
- [x] `VonMisesDistribution`
- [x] `VonMisesStress`
- [x] `WaitAll`
- [x] `WaitAsynchronousTask`
- [x] `WaitNext`
- [x] `WakebyDistribution`
- [x] `WalleniusHypergeometricDistribution`
- [x] `WaringYuleDistribution`
- [x] `WarpingCorrespondence`
- [x] `WarpingDistance`
- [x] `WatershedComponents`
- [x] `WatsonUSquareTest`
- [x] `WattsStrogatzGraphDistribution`
- [x] `WaveletBestBasis`
- [x] `WaveletFilterCoefficients`
- [x] `WaveletMapIndexed`
- [x] `WaveletPhi`
- [x] `WaveletPsi`
- [x] `WaveletScalogram`
- [x] `WaveletThreshold`
- [x] `WavePDEComponent`

### Batch 79: WeaklyConnectedComponents to WignerD

**Status:** Pending

- [x] `WeaklyConnectedComponents`
- [x] `WeaklyConnectedGraphComponents`
- [x] `WeakStationarity`
- [x] `WeatherData`
- [x] `WeatherForecastData`
- [x] `WebAudioSearch`
- [x] `WebColumn`
- [x] `WebElementObject`
- [x] `WeberE`
- [x] `WebExecute`
- [x] `WebImage`
- [x] `WebImageSearch`
- [x] `WebItem`
- [x] `WebRow`
- [x] `WebSearch`
- [x] `WebSessionObject`
- [x] `WebSessions`
- [x] `WebWindowObject`
- [x] `Wedge`
- [x] `WeibullDistribution`
- [x] `WeierstrassE1`
- [x] `WeierstrassE2`
- [x] `WeierstrassE3`
- [x] `WeierstrassEta1`
- [x] `WeierstrassEta2`
- [x] `WeierstrassEta3`
- [x] `WeierstrassHalfPeriods`
- [x] `WeierstrassHalfPeriodW1`
- [x] `WeierstrassHalfPeriodW2`
- [x] `WeierstrassHalfPeriodW3`
- [x] `WeierstrassInvariantG2`
- [x] `WeierstrassInvariantG3`
- [x] `WeierstrassInvariants`
- [x] `WeierstrassP`
- [x] `WeierstrassPPrime`
- [x] `WeierstrassSigma`
- [x] `WeierstrassZeta`
- [x] `WeightedAdjacencyGraph`
- [x] `WeightedData`
- [x] `WelchWindow`
- [x] `WeylAlgebra`
- [x] `WheelGraph`
- [x] `WhenEvent`
- [x] `While`
- [x] `WhiteNoiseProcess`
- [x] `WhittakerM`
- [x] `WhittakerW`
- [x] `WienerFilter`
- [x] `WienerProcess`
- [x] `WignerD`

### Batch 80: WignerSemicircleDistribution to ZTransform

**Status:** Pending

- [x] `WignerSemicircleDistribution`
- [x] `WikidataData`
- [x] `WikidataSearch`
- [x] `WikipediaData`
- [x] `WikipediaSearch`
- [x] `WilksW`
- [x] `WilksWTest`
- [x] `WindDirectionData`
- [x] `WindingPolygon`
- [x] `WindSpeedData`
- [x] `WindVectorData`
- [x] `WinsorizedMean`
- [x] `WinsorizedVariance`
- [x] `WithCleanup`
- [x] `WithLock`
- [x] `WolframAlpha`
- [x] `WolframLanguageData`
- [x] `WordCloud`
- [x] `WordCounts`
- [x] `WordData`
- [x] `WordDefinition`
- [x] `WordFrequency`
- [x] `WordFrequencyData`
- [x] `WordStem`
- [x] `WordTranslation`
- [x] `Write`
- [x] `WriteLine`
- [x] `WriteString`
- [x] `Wronskian`
- [x] `XMLElement`
- [x] `XMLObject`
- [x] `XMLTemplate`
- [x] `Xnor`
- [x] `YuleDissimilarity`
- [x] `ZernikeR`
- [x] `ZeroSymmetric`
- [x] `Zeta`
- [x] `ZetaZero`
- [x] `ZIPCodeData`
- [x] `ZipfDistribution`
- [x] `ZTest`
- [x] `ZTransform`
