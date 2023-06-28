<!DOCTYPE qgis PUBLIC 'http://mrcc.com/qgis.dtd' 'SYSTEM'>
<qgis maxScale="0" styleCategories="AllStyleCategories" minScale="1e+08" hasScaleBasedVisibilityFlag="0" version="3.22.1-Białowieża">
  <flags>
    <Identifiable>1</Identifiable>
    <Removable>1</Removable>
    <Searchable>1</Searchable>
    <Private>0</Private>
  </flags>
  <temporal fetchMode="0" enabled="0" mode="0">
    <fixedRange>
      <start></start>
      <end></end>
    </fixedRange>
  </temporal>
  <customproperties>
    <Option type="Map">
      <Option type="bool" name="WMSBackgroundLayer" value="false"/>
      <Option type="bool" name="WMSPublishDataSourceUrl" value="false"/>
      <Option type="int" name="embeddedWidgets/count" value="0"/>
      <Option type="QString" name="identify/format" value="Value"/>
    </Option>
  </customproperties>
  <pipe-data-defined-properties>
    <Option type="Map">
      <Option type="QString" name="name" value=""/>
      <Option name="properties"/>
      <Option type="QString" name="type" value="collection"/>
    </Option>
  </pipe-data-defined-properties>
  <pipe>
    <provider>
      <resampling zoomedInResamplingMethod="nearestNeighbour" maxOversampling="2" zoomedOutResamplingMethod="nearestNeighbour" enabled="false"/>
    </provider>
    <rasterrenderer alphaBand="-1" opacity="1" type="paletted" band="1" nodataColor="">
      <rasterTransparency/>
      <minMaxOrigin>
        <limits>None</limits>
        <extent>WholeRaster</extent>
        <statAccuracy>Estimated</statAccuracy>
        <cumulativeCutLower>0.02</cumulativeCutLower>
        <cumulativeCutUpper>0.98</cumulativeCutUpper>
        <stdDevFactor>2</stdDevFactor>
      </minMaxOrigin>
      <colorPalette>
        <paletteEntry color="#000000" label="000 No Data" alpha="255" value="0"/>
        <paletteEntry color="#ffff64" label="010 Cropland, rainfed" alpha="255" value="10"/>
        <paletteEntry color="#aaf0f0" label="020 Cropland, irrigated or post-flooding" alpha="255" value="20"/>
        <paletteEntry color="#dcf064" label="030 Mosaic cropland (>50%) / natural vegettion (tree, shrub, herbaceous cover) (&lt;50%)" alpha="255" value="30"/>
        <paletteEntry color="#c8c864" label="040 Mosaic natural vegetation (tree, shrub, herbaceous cover) (>50%) / cropland (&lt;50%)" alpha="255" value="40"/>
        <paletteEntry color="#006400" label="050 Tree cover, broadleaved, evergreen, closed to open (>15%)" alpha="255" value="50"/>
        <paletteEntry color="#00a000" label="060 Tree cover, broadleaved, decidous, closed to open (>15%)" alpha="255" value="60"/>
        <paletteEntry color="#003c00" label="070 Tree cover, needleleaved, evergreen, closed to open (>15%)" alpha="255" value="70"/>
        <paletteEntry color="#285000" label="080 Tree cover, needleleaved, decidous, closed to open (>15%)" alpha="255" value="80"/>
        <paletteEntry color="#788300" label="090 Tree cover, mixed leaf type (broadleaved and needleleaved)" alpha="255" value="90"/>
        <paletteEntry color="#8da000" label="100 Mosaic tree and shrub (>50%) / herbaceous cover (&lt;50%)" alpha="255" value="100"/>
        <paletteEntry color="#be9600" label="110 Mosaic herbaceous cover (>50%) / tree and shrub (&lt;50%)" alpha="255" value="110"/>
        <paletteEntry color="#966400" label="120 Shurbland" alpha="255" value="120"/>
        <paletteEntry color="#ffb432" label="130 Grassland" alpha="255" value="130"/>
        <paletteEntry color="#ffdcd2" label="140 Lichens and mosses" alpha="255" value="140"/>
        <paletteEntry color="#ffebaf" label="150 Sparse vegetation (tree, shurb, herbaceous cover) (&lt;15%)" alpha="255" value="150"/>
        <paletteEntry color="#00785a" label="160 Tree cover, flooded, fresh or brakish water" alpha="255" value="160"/>
        <paletteEntry color="#009678" label="170 Tree cover, flooded, saline water" alpha="255" value="170"/>
        <paletteEntry color="#00dc83" label="180 Shub or herbaceous cover, flooded, fresh/saline/brakish water" alpha="255" value="180"/>
        <paletteEntry color="#c31400" label="190 Urban areas" alpha="255" value="190"/>
        <paletteEntry color="#fff5d7" label="Bare areas" alpha="255" value="200"/>
        <paletteEntry color="#0046c8" label="210 Water bodies" alpha="255" value="210"/>
        <paletteEntry color="#ffffff" label="220 Permanent snow and ice" alpha="255" value="220"/>
      </colorPalette>
      <colorramp type="randomcolors" name="[source]">
        <Option/>
      </colorramp>
    </rasterrenderer>
    <brightnesscontrast contrast="0" brightness="0" gamma="1"/>
    <huesaturation colorizeOn="0" invertColors="0" saturation="0" colorizeGreen="128" grayscaleMode="0" colorizeRed="255" colorizeStrength="100" colorizeBlue="128"/>
    <rasterresampler maxOversampling="2"/>
    <resamplingStage>resamplingFilter</resamplingStage>
  </pipe>
  <blendMode>0</blendMode>
</qgis>
