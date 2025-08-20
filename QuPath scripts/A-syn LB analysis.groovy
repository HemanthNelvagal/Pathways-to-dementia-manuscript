// Set colour deconvolution - Extrema 1%
setColorDeconvolutionStains('{"Name" : "H-DAB estimated1", "Stain 1" : "Hematoxylin", "Values 1" : "0.63907 0.65543 0.4025", "Stain 2" : "DAB", "Values 2" : "0.31846 0.56828 0.75871", "Background" : " 255 255 255"}');

// StarDist
/* PARAMETERS */
def modelFolder = "/path/to/StarDist/OpenCV_converted_models" // CHANGE THIS
String channel = "DAB"  // "RGB" for BF model ; "HTX", "DAB", "Residual" for deconvolved BF ; use channel name for FL ; "Average":Mean of all channels for BF/FL
double downsampleFactor = 2 // Image downsample factor
def normalisationRange = [95, 100] // Increase min percentile to exclude lower intensity objects; decrease max percentile to exclude higher intensity objects
double gaussianSigma = 1.0 // Sigma value for Gaussian blur
double threshold = 0.5 // Probability threshold for detection
int tileSize = 2000 // In pixels
double cellExpansionMicrons = 0 // Cell expansion in microns
boolean excludeEdges = true // Option to exclude detections at the edge of parent annotations and custom tiles

def stardistParentAnno = getAnnotationObjects().findAll{
    it.getPathClass() != getPathClass("Ignore*") &&
    it.getPathClass() != getPathClass("!BACKGROUND")
}

// If parent annotations present, run custom StarDist
if (stardistParentAnno) {
    customStarDist(stardistParentAnno, modelFolder, channel, downsampleFactor, normalisationRange, gaussianSigma, threshold, tileSize, cellExpansionMicrons, overwrite, excludeEdges)
} else {
    logger.warn("No parent annotations found")
}

// Get area surrounding detection objects
def hierarchy = getCurrentHierarchy()
double pixelSize1 = getCurrentImageData().getServer().getPixelCalibration().getAveragedPixelSize()
def micronToDilate = 2
def pixelToDilate = micronToDilate/pixelSize1
def nucleusScale = -1 // maximum size of cell relative to nucleus (ignored if <= 1)

logger.info("Dilating all detections by $micronToDilate µm")
def analysisAnno = getAnnotationObjects()
for (def anno in analysisAnno) {
    def pathObjects = hierarchy.getObjectsForROI(null, anno.getROI()).findAll{it.isDetection()}
    def expandedObj = CellTools.detectionsToCells(pathObjects, pixelToDilate, nucleusScale)
    addObjects(expandedObj)
    removeObjects(pathObjects, true)
}

// Add intensity and shape measurements to objects
import qupath.lib.images.servers.TransformedServerBuilder
import qupath.lib.analysis.features.ObjectMeasurements
import qupath.lib.analysis.features.ObjectMeasurements.Measurements
import qupath.lib.analysis.features.ObjectMeasurements.Compartments

def imageData = getCurrentImageData()
def server = getCurrentServer()
def stains = imageData.getColorDeconvolutionStains()
def deconvServer1 = new TransformedServerBuilder(server).deconvolveStains(stains, 1,2,3).build()
def cal = server.getPixelCalibration()
def pathObjects = getDetectionObjects()
def downsample = 1
def intensityMeasurements = [Measurements.MEAN, Measurements.MEDIAN, Measurements.MIN, Measurements.MAX, Measurements.STD_DEV]
def compartments = [Compartments.CELL, Compartments.CYTOPLASM, Compartments.NUCLEUS]

logger.info("Measuring intensity and shape features for all objects")
pathObjects.parallelStream().forEach{ obj ->
    ObjectMeasurements.addIntensityMeasurements(server, obj, downsample, intensityMeasurements, compartments)
    ObjectMeasurements.addIntensityMeasurements(deconvServer1, obj, downsample, intensityMeasurements, compartments)
    ObjectMeasurements.addShapeMeasurements(obj, cal) // All shape features used
}

selectObjects(pathObjects)
runPlugin('qupath.lib.algorithms.HaralickFeaturesPlugin', '{"downsample": "1", "magnification": "40", "stainChoice": "H-DAB", "tileSizeMicrons": "0", "includeStats": "false", "useNucleusROIs": "true", "haralickDistance": "1", "haralickBins": "32"}')

runObjectClassifier("/path/to/object_classifiers/Lewy body object classifier 11.json")

/* IMPORTS */
import qupath.lib.images.servers.TransformedServerBuilder
import qupath.ext.stardist.StarDist2D
import qupath.lib.roi.RoiTools
import qupath.lib.geom.ImmutableDimension
import qupath.lib.images.ImageData
import qupath.lib.regions.ImageRegion
import org.locationtech.jts.geom.util.LinearComponentExtracter
import qupath.lib.scripting.QP

/* FUNCTIONS */
def customStarDist(annotations, modelFolder, channel, downsampleFactor, normalisationRange, gaussianSigma, threshold, tileSize, cellExpansionMicrons, excludeEdges) {
    def hierarchy = getCurrentHierarchy()
    def imageData = getCurrentImageData()
    def imageType = imageData.getImageType()
    def server = imageData.getServer()
    def cal = server.getPixelCalibration()
    def pathModel

    if (imageType.toString().contains("Brightfield")) {
        if (channel == "RGB") {
            logger.info("Using he_heavy_augment.pb model")
            pathModel = buildFilePath(modelFolder, "he_heavy_augment.pb")
        } else {
            logger.info("Using dsb2018_heavy_augment.pb model")
            pathModel = buildFilePath(modelFolder, "dsb2018_heavy_augment.pb")
            def stains = imageData.getColorDeconvolutionStains()

            if (channel == "HTX") {
                server = new TransformedServerBuilder(server).deconvolveStains(stains, 1).build()
            } else if (channel == "DAB") {
                server = new TransformedServerBuilder(server).deconvolveStains(stains, 2).build()
            } else if (channel == "Residual") {
                server = new TransformedServerBuilder(server).deconvolveStains(stains, 3).build()
            } else if (channel == "Average") {
                server = new TransformedServerBuilder(server).averageChannelProject().build()
            }
            imageData = new ImageData<>(server, hierarchy, imageType)
        }
    } else if (imageType.toString() == "Fluorescence") {
        logger.info("Using dsb2018_heavy_augment.pb model")
        pathModel = buildFilePath(modelFolder, "dsb2018_heavy_augment.pb")
        if (channel == "Average") {
            server = new TransformedServerBuilder(server).averageChannelProject().build()
        } else {
            server = new TransformedServerBuilder(server).extractChannels(channel).build()
        }
        imageData = new ImageData<>(server, hierarchy, imageType)
    } else {
        logger.error("Current image type not compatible with custom StarDist.")
        return
    }

    double normalMin = normalisationRange[0]
    double normalMax = normalisationRange[1]
    double pixelSize = cal.getAveragedPixelSize()*downsampleFactor
    double cellExpansionPixels = cellExpansionMicrons/pixelSize

    def stardist = StarDist2D.builder(pathModel)
        .preprocess(
            StarDist2D.imageNormalizationBuilder() // Global normalisation
                .perChannel(true)
                .maxDimension(tileSize)
                .percentiles(normalMin, normalMax)
                .useMask(true) // Limit normalization calculation to ROI mask
                .build()
        )
        .preprocess(
            ImageOps.Filters.gaussianBlur(gaussianSigma)
        )
        .threshold(threshold)              // Probability (detection) threshold
        .pixelSize(pixelSize)
        .tileSize(tileSize)              // Specify width & height of the tile used for prediction
        .cellExpansion(cellExpansionPixels)          // Approximate cells based upon nucleus expansion
        .includeProbability(true)    // Add probability as a measurement (enables later filtering)
        .build()

    // Use QuPath StarDist's tiling
    stardist.detectObjects(imageData, annotations)
    stardist.close()

    if (excludeEdges) {
        annotations.forEach{ anno ->
            excludeObjectsTouchingRoiEdge(anno, 0.1, false)
        }
    }
    logger.info("StarDist complete")
}

def excludeObjectsTouchingRoiEdge(pathObject, distancePixels, useHierarchyRule) {
    def hierarchy = getCurrentHierarchy()
    def roi = pathObject.getROI()

    def childObjects
    if (useHierarchyRule) {
        childObjects = hierarchy.getObjectsForRegion(null, ImageRegion.createInstance(roi), null).findAll{it != pathObject && it.getROI().getArea() < roi.getArea()} // exclude itself and equal or larger objects
    } else {
        childObjects = hierarchy.getObjectsForROI(null, roi).findAll{it != pathObject && it.getROI().getArea() < roi.getArea()} // exclude itself and equal or larger objects
    }

    def geometry = roi.getGeometry()
    def geometryLines = LinearComponentExtracter.getLines(geometry)
    def toRemove = geometryLines.collectMany { line ->
        return childObjects.findAll { line.isWithinDistance(it.getROI().getGeometry(), distancePixels) }
    }
    
    removeObjects(toRemove, true)
}
