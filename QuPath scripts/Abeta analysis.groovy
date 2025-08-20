// Set colour deconvolution - Extrema 1%
setColorDeconvolutionStains('{"Name" : "H-DAB estimated1", "Stain 1" : "Hematoxylin", "Values 1" : "0.63907 0.65543 0.4025", "Stain 2" : "DAB", "Values 2" : "0.31846 0.56828 0.75871", "Background" : " 255 255 255"}')

// Measure DAB intensity in !BACKGROUND annotations
import qupath.lib.images.servers.TransformedServerBuilder
import qupath.lib.analysis.features.ObjectMeasurements
import qupath.lib.analysis.features.ObjectMeasurements.Measurements
import qupath.lib.analysis.features.ObjectMeasurements.Compartments

def imageData = getCurrentImageData()
def server = getCurrentServer()
def stains = imageData.getColorDeconvolutionStains()
def deconvServer = new TransformedServerBuilder(server).deconvolveStains(stains, 1,2,3).build()
def downsample = 2
def intensityMeasurements = [Measurements.MEAN, Measurements.MEDIAN, Measurements.MIN, Measurements.MAX, Measurements.STD_DEV]
def compartments = [Compartments.CELL, Compartments.CYTOPLASM, Compartments.NUCLEUS]

def bgAnno = getAnnotationObjects().find{it.getPathClass() == getPathClass("!BACKGROUND")}

def dabBgAll
def bgSplitAnno
if (bgAnno) {
    logger.info("Measuring DAB intensity in all !BACKGROUND annotations")
    ObjectMeasurements.addIntensityMeasurements(deconvServer, bgAnno, downsample, intensityMeasurements, compartments)
    dabBgAll = bgAnno.measurements.get("DAB: Mean")
    
    // Unmerge !BACKGROUND annotations
    def bgRoi = bgAnno.getROI()
    bgRoi = RoiTools.splitROI(bgRoi)

    bgSplitAnno = bgRoi.collect{roi -> PathObjects.createAnnotationObject(roi, bgAnno.getPathClass())}
    logger.info("Measuring DAB intensity in individual !BACKGROUND annotations")
    bgSplitAnno.parallelStream().forEach{
        it.setName(bgAnno.getName())
        it.setLocked(true)
        ObjectMeasurements.addIntensityMeasurements(deconvServer, it, downsample, intensityMeasurements, compartments)
    }
    removeObject(bgAnno, true)
    addObjects(bgSplitAnno)
} else {
    logger.warn("No !BACKGROUND annotation found in ${getCurrentImageNameWithoutExtension()}")
}



// Apply fixed and auto threshold based on DAB intensity in !BACKGROUND
// Auto threshold
/* PARAMETERS */
String channel = "DAB" // "HTX", "DAB", "Residual" for BF ; use channel name for FL ; "Average":Mean of all channels for BF/FL
def threshold = "Triangle" // "Default", "Huang", "Intermodes", "IsoData", "IJ_IsoData", "Li", "MaxEntropy", "Mean", "MinError", "Minimum", "Moments", "Otsu", "Percentile", "RenyiEntropy", "Shanbhag", "Triangle", "Yen"
double thresholdDownsample = 8 // 1:Full, 2:Very high, 4:High, 8:Moderate, 16:Low, 32:Very low, 64:Extremely low
boolean darkBackground = false // Adapt threshold method for dark backgrounds
def thresholdFloor = 0.25 // Set a threshold floor value in case auto threshold is too low. Set null to disable
String output = "measurement" // "annotation", "detection", "measurement", "threshold value"

double classifierDownsample = 2 // 1:Full, 2:Very high, 4:High, 8:Moderate, 16:Low, 32:Very low, 64:Extremely low
double classifierGaussianSigma = 0.5 // Strength of gaussian blurring for pixel classifier (not used in calculation of threshold)
String classBelow = null // null or "Class Name"; use this for positive "Average" channel on brightfield
String classAbove = "Abeta" // null or "Class Name"; use this for positive deconvoluted or fluorescence channels

/* Create object parameters */
double minArea = 0 // Minimum area for annotations to be created
double minHoleArea = 0 // Minimum area for holes in annotations to be created
String classifierObjectOptions = "" // "SPLIT,DELETE_EXISTING,INCLUDE_IGNORED,SELECT_NEW"


def annoList = getAnnotationObjects().findAll{
    it.getPathClass() != getPathClass("Ignore*") && 
    it.getPathClass() != getPathClass("All annotations") && 
    it.getPathClass() != getPathClass("!BACKGROUND") && 
    !it.getPathClass().toString().contains("Deep") && 
    !it.getPathClass().toString().contains("Crest")
}


if (annoList) {
    def hierarchy = getCurrentHierarchy()
    annoList.forEach{ anno ->
        // Apply normal Otsu auto threshold to get threshold value and area measurement
        autoThreshold(anno, channel, threshold, thresholdDownsample, darkBackground, thresholdFloor, output, classifierDownsample, classifierGaussianSigma, classBelow, classAbove, minArea, minHoleArea, classifierObjectOptions)
        String measurementName = "${threshold}: ${classAbove} threshold value"
        double autoThresholdValue = anno.measurements.get(measurementName.toString())

        // Calculate background adjusted thresholds
        def bgChildAnno = hierarchy.getObjectsForROI(null, anno.getROI()).find{it.getPathClass() == getPathClass("!BACKGROUND")}
        double dabBgMean
        if (bgChildAnno) {
            dabBgMean = bgChildAnno.measurements.get("DAB: Mean")
        } else {
            logger.info("No !BACKGROUND annotation found in ${anno.getName()}. Using all !BACKGROUND mean DAB intensity.")
            dabBgMean = dabBgAll
        }
                
        double fixedThresholdValueBgCorrected = dabBgMean + 0.25
        double autoThresholdValueBgCorrected = dabBgMean + autoThresholdValue

        anno.measurements.put("DAB: Mean (BG)", dabBgMean)
        anno.measurements.put("DAB: Mean (BG) + Fixed: ${classAbove} threshold value", fixedThresholdValueBgCorrected)
        anno.measurements.put("DAB: Mean (BG) + ${measurementName.toString()}", autoThresholdValueBgCorrected)


        def areaAbetaMeasurementName = "Fixed threshold: ${classAbove} area µm^2"

        // Apply background adjusted fixed threshold
        autoThreshold(anno, channel, fixedThresholdValueBgCorrected, thresholdDownsample, darkBackground, thresholdFloor, output, classifierDownsample, classifierGaussianSigma, classBelow, classAbove, minArea, minHoleArea, classifierObjectOptions)
        def fixedThresholdBgCorrectedAbetaArea = anno.measurements.get(areaAbetaMeasurementName.toString())
        anno.measurements.put("BG corrected Fixed threshold: ${classAbove} area µm^2", fixedThresholdBgCorrectedAbetaArea)

        // Apply background adjusted auto threshold
        autoThreshold(anno, channel, autoThresholdValueBgCorrected, thresholdDownsample, darkBackground, thresholdFloor, output, classifierDownsample, classifierGaussianSigma, classBelow, classAbove, minArea, minHoleArea, classifierObjectOptions)
        def autoThresholdBgCorrectedAbetaArea = anno.measurements.get(areaAbetaMeasurementName.toString())
        anno.measurements.put("BG corrected ${threshold} threshold: ${classAbove} area µm^2", autoThresholdBgCorrectedAbetaArea)

        // Apply normal fixed threshold
        autoThreshold(anno, channel, 0.25, thresholdDownsample, darkBackground, thresholdFloor, output, classifierDownsample, classifierGaussianSigma, classBelow, classAbove, minArea, minHoleArea, classifierObjectOptions)
    }
} else {
    logger.warn("No annotations selected.")
}

/* IMPORTS */
import qupath.lib.images.servers.TransformedServerBuilder
import qupath.lib.roi.interfaces.ROI
import qupath.imagej.tools.IJTools
import qupath.lib.images.PathImage
import qupath.lib.regions.RegionRequest
import ij.ImagePlus
import ij.process.ImageProcessor
import qupath.opencv.ml.pixel.PixelClassifiers
import qupath.lib.gui.viewer.OverlayOptions
import qupath.lib.gui.viewer.RegionFilter
import qupath.lib.gui.viewer.overlays.PixelClassificationOverlay
import qupath.lib.images.servers.ColorTransforms.ColorTransform
import qupath.opencv.ops.ImageOp
import qupath.opencv.ops.ImageOps

/* FUNCTIONS */
def autoThreshold(annotation, channel, threshold, thresholdDownsample, darkBackground, thresholdFloor, output, classifierDownsample, classifierGaussianSigma, classBelow, classAbove, minArea, minHoleArea, classifierObjectOptions) {
    def qupath = getQuPath()
    def imageData = getCurrentImageData()
    def imageType = imageData.getImageType()
    def server = imageData.getServer()
    def cal = server.getPixelCalibration()
    def resolution = cal.createScaledInstance(classifierDownsample, classifierDownsample)
    def classifierChannel

    if (imageType.toString().contains("Brightfield")) {
        def stains = imageData.getColorDeconvolutionStains()

        if (channel == "HTX") {
            server = new TransformedServerBuilder(server).deconvolveStains(stains, 1).build()
            classifierChannel = ColorTransforms.createColorDeconvolvedChannel(stains, 1)
        } else if (channel == "DAB") {
            server = new TransformedServerBuilder(server).deconvolveStains(stains, 2).build()
            classifierChannel = ColorTransforms.createColorDeconvolvedChannel(stains, 2)
        } else if (channel == "Residual") {
            server = new TransformedServerBuilder(server).deconvolveStains(stains, 3).build()
            classifierChannel = ColorTransforms.createColorDeconvolvedChannel(stains, 3)
        } else if (channel == "Average") {
            server = new TransformedServerBuilder(server).averageChannelProject().build()
            classifierChannel = ColorTransforms.createMeanChannelTransform()
        }
    } else if (imageType.toString() == "Fluorescence") {
        if (channel == "Average") {
            server = new TransformedServerBuilder(server).averageChannelProject().build()
            classifierChannel = ColorTransforms.createMeanChannelTransform()
        } else {
            server = new TransformedServerBuilder(server).extractChannels(channel).build()
            classifierChannel = ColorTransforms.createChannelExtractor(channel)
        }
    } else {
        logger.error("Current image type not compatible with auto threshold.")
        return
    }

    // Check if threshold is Double (for fixed threshold) or String (for auto threshold)
    String thresholdMethod
    if (threshold instanceof String) {
        thresholdMethod = threshold
    } else {
        thresholdMethod = "Fixed"
    }

    // Apply the selected algorithm
    def validThresholds = ["Fixed", "Default", "Huang", "Intermodes", "IsoData", "IJ_IsoData", "Li", "MaxEntropy", "Mean", "MinError", "Minimum", "Moments", "Otsu", "Percentile", "RenyiEntropy", "Shanbhag", "Triangle", "Yen"]

    double thresholdValue
    if (thresholdMethod in validThresholds){
        if (thresholdMethod == "Fixed") {
            thresholdValue = threshold
        } else {
            // Determine threshold value by auto threshold method
            ROI pathROI = annotation.getROI() // Get QuPath ROI
            PathImage pathImage = IJTools.convertToImagePlus(server, RegionRequest.createInstance(server.getPath(), thresholdDownsample, pathROI)) // Get PathImage within bounding box of annotation
            def ijRoi = IJTools.convertToIJRoi(pathROI, pathImage) // Convert QuPath ROI into ImageJ ROI
            ImagePlus imagePlus = pathImage.getImage() // Convert PathImage into ImagePlus
            ImageProcessor ip = imagePlus.getProcessor() // Get ImageProcessor from ImagePlus
            ip.setRoi(ijRoi) // Add ImageJ ROI to the ImageProcessor to limit the histogram to within the ROI only

            if (darkBackground) {
                ip.setAutoThreshold("${thresholdMethod} dark")
            } else {
                ip.setAutoThreshold("${thresholdMethod}")
            }

            thresholdValue = ip.getMaxThreshold()
            if (thresholdValue != null && thresholdValue < thresholdFloor) {
                thresholdValue = thresholdFloor
            }
        }
    } else {
        logger.error("Invalid auto-threshold method")
        return
    }

    // If specified output is "threshold value, return threshold value in annotation measurements
    if (output == "threshold value") {
        logger.info("${thresholdMethod} threshold value: ${thresholdValue}")
        annotation.measurements.put("${thresholdMethod} threshold value", thresholdValue)
        return
    }

    // Assign classification
    def classificationBelow
    if (classBelow instanceof PathClass) {
        classificationBelow = classBelow
    } else if (classBelow instanceof String) {
        classificationBelow = getPathClass(classBelow)
    } else if (classBelow == null) {
        classificationBelow = classBelow
    }
    
    def classificationAbove
    if (classAbove instanceof PathClass) {
        classificationAbove = classAbove
    } else if (classAbove instanceof String) {
        classificationAbove = getPathClass(classAbove)
    } else if (classAbove == null) {
        classificationAbove = classAbove
    }

    Map<Integer, PathClass> classifications = new LinkedHashMap<>()
    classifications.put(0, classificationBelow)
    classifications.put(1, classificationAbove)

    // Define parameters for pixel classifier
    List<ImageOp> ops = new ArrayList<>()
    ops.add(ImageOps.Filters.gaussianBlur(classifierGaussianSigma))
    ops.add(ImageOps.Threshold.threshold(thresholdValue))

    // Create pixel classifier
    def op = ImageOps.Core.sequential(ops)
    def transformer = ImageOps.buildImageDataOp(classifierChannel).appendOps(op)
    def classifier = PixelClassifiers.createClassifier(
        transformer,
        resolution,
        classifications
    )

    // Apply classifier
    selectObjects(annotation)
    if (output == "annotation") {
        logger.info("Creating annotations in ${annotation} from ${thresholdMethod}: ${thresholdValue}")
        
        if (classifierObjectOptions) {
            classifierObjectOptions = classifierObjectOptions.split(',')
            def allowedOptions = ["SPLIT", "DELETE_EXISTING", "INCLUDE_IGNORED", "SELECT_NEW"]
            boolean checkValid = classifierObjectOptions.every{allowedOptions.contains(it)}

            if (checkValid) {
                createAnnotationsFromPixelClassifier(classifier, minArea, minHoleArea, classifierObjectOptions)
            } else {
                logger.warn("Invalid create annotation options")
                return
            }
        } else {
            createAnnotationsFromPixelClassifier(classifier, minArea, minHoleArea)
        }
    }
    if (output == "detection") {
        logger.info("Creating detections in ${annotation} from ${thresholdMethod}: ${thresholdValue}")

        if (classifierObjectOptions) {
            classifierObjectOptions = classifierObjectOptions.split(',')
            def allowedOptions = ["SPLIT", "DELETE_EXISTING", "INCLUDE_IGNORED", "SELECT_NEW"]
            boolean checkValid = classifierObjectOptions.every{allowedOptions.contains(it)}

            if (checkValid) {
                createDetectionsFromPixelClassifier(classifier, minArea, minHoleArea, classifierObjectOptions)
            } else {
                logger.warn("Invalid create detection options")
                return
            }
        } else {
            createDetectionsFromPixelClassifier(classifier, minArea, minHoleArea)
        }
    }
    if (output == "measurement") {
        logger.info("Measuring thresholded area in ${annotation} from ${thresholdMethod}: ${thresholdValue}")
        def measurementID = "${thresholdMethod} threshold"
        addPixelClassifierMeasurements(classifier, measurementID)
    }
    if (output == "preview") {
        logger.info("Showing preview of ${annotation} with ${thresholdMethod}: ${thresholdValue}")
        OverlayOptions overlayOption = qupath.getOverlayOptions()
        overlayOption.setPixelClassificationRegionFilter(RegionFilter.StandardRegionFilters.ANY_ANNOTATIONS) // RegionFilter.StandardRegionFilters.ANY_ANNOTATIONS
        PixelClassificationOverlay previewOverlay = PixelClassificationOverlay.create(overlayOption, classifier)
        previewOverlay.setLivePrediction(true)
        qupath.getViewer().setCustomPixelLayerOverlay(previewOverlay)
    }
    
    if (classificationBelow == null) {
        annotation.measurements.put("${thresholdMethod}: ${classificationAbove.toString()} threshold value", thresholdValue)
    }
    if (classificationAbove == null) {
        annotation.measurements.put("${thresholdMethod}: ${classificationBelow.toString()} threshold value", thresholdValue)
    }
    if (classificationBelow != null && classificationAbove != null) {
        annotation.measurements.put("${thresholdMethod} threshold value", thresholdValue)
    }
}