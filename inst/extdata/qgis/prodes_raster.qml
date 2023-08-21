<!DOCTYPE qgis PUBLIC 'http://mrcc.com/qgis.dtd' 'SYSTEM'>
<qgis version="3.22.7-Białowieża" maxScale="0" minScale="1e+08" hasScaleBasedVisibilityFlag="0" styleCategories="AllStyleCategories">
  <flags>
    <Identifiable>1</Identifiable>
    <Removable>1</Removable>
    <Searchable>1</Searchable>
    <Private>0</Private>
  </flags>
  <temporal mode="0" fetchMode="0" enabled="0">
    <fixedRange>
      <start></start>
      <end></end>
    </fixedRange>
  </temporal>
  <customproperties>
    <Option type="Map">
      <Option value="false" type="bool" name="WMSBackgroundLayer"/>
      <Option value="false" type="bool" name="WMSPublishDataSourceUrl"/>
      <Option value="0" type="int" name="embeddedWidgets/count"/>
      <Option value="Value" type="QString" name="identify/format"/>
    </Option>
  </customproperties>
  <pipe-data-defined-properties>
    <Option type="Map">
      <Option value="" type="QString" name="name"/>
      <Option name="properties"/>
      <Option value="collection" type="QString" name="type"/>
    </Option>
  </pipe-data-defined-properties>
  <pipe>
    <provider>
      <resampling maxOversampling="2" zoomedInResamplingMethod="nearestNeighbour" zoomedOutResamplingMethod="nearestNeighbour" enabled="false"/>
    </provider>
    <rasterrenderer alphaBand="-1" classificationMax="102" opacity="1" classificationMin="7" nodataColor="" band="1" type="singlebandpseudocolor">
      <rasterTransparency/>
      <minMaxOrigin>
        <limits>None</limits>
        <extent>WholeRaster</extent>
        <statAccuracy>Estimated</statAccuracy>
        <cumulativeCutLower>0.02</cumulativeCutLower>
        <cumulativeCutUpper>0.98</cumulativeCutUpper>
        <stdDevFactor>2</stdDevFactor>
      </minMaxOrigin>
      <rastershader>
        <colorrampshader classificationMode="2" clip="0" minimumValue="7" labelPrecision="0" colorRampType="INTERPOLATED" maximumValue="102">
          <colorramp type="gradient" name="[source]">
            <Option type="Map">
              <Option value="231,231,55,255" type="QString" name="color1"/>
              <Option value="0,0,0,255" type="QString" name="color2"/>
              <Option value="0" type="QString" name="discrete"/>
              <Option value="gradient" type="QString" name="rampType"/>
              <Option value="0.0105263;236,104,65,255:0.0210526;241,124,74,255:0.0315789;246,144,83,255:0.0421053;251,164,92,255:0.0526316;254,180,103,255:0.0631579;254,190,116,255:0.0736842;254,201,128,255:0.0842105;254,212,141,255:0.0947368;255,223,154,255:0.105263;255,234,166,255:0.115789;255,245,179,255:0.126316;255,255,191,255:0.136842;244,251,188,255:0.147368;233,246,184,255:0.263158;112,219,242,255:0.452632;199,233,173,255:0.463158;188,228,170,255:0.473684;163,215,166,255:0.484211;111,179,174,255:0.494737;145,203,169,255:0.505263;211,237,177,255:0.515789;128,191,172,255:0.526316;77,155,180,255:0.536842;114,187,178,255:0.547368;94,167,177,255:0.557895;60,143,183,255:0.568421;43,131,186,255:0.884211;37,67,220,255:0.978947;11,105,82,255:0.989474;232,83,245,255" type="QString" name="stops"/>
            </Option>
            <prop k="color1" v="231,231,55,255"/>
            <prop k="color2" v="0,0,0,255"/>
            <prop k="discrete" v="0"/>
            <prop k="rampType" v="gradient"/>
            <prop k="stops" v="0.0105263;236,104,65,255:0.0210526;241,124,74,255:0.0315789;246,144,83,255:0.0421053;251,164,92,255:0.0526316;254,180,103,255:0.0631579;254,190,116,255:0.0736842;254,201,128,255:0.0842105;254,212,141,255:0.0947368;255,223,154,255:0.105263;255,234,166,255:0.115789;255,245,179,255:0.126316;255,255,191,255:0.136842;244,251,188,255:0.147368;233,246,184,255:0.263158;112,219,242,255:0.452632;199,233,173,255:0.463158;188,228,170,255:0.473684;163,215,166,255:0.484211;111,179,174,255:0.494737;145,203,169,255:0.505263;211,237,177,255:0.515789;128,191,172,255:0.526316;77,155,180,255:0.536842;114,187,178,255:0.547368;94,167,177,255:0.557895;60,143,183,255:0.568421;43,131,186,255:0.884211;37,67,220,255:0.978947;11,105,82,255:0.989474;232,83,245,255"/>
          </colorramp>
          <item label="7 d2007" alpha="255" color="#e7e737" value="7"/>
          <item label="8 d2008" alpha="255" color="#ec6841" value="8"/>
          <item label="9 d2009" alpha="255" color="#f17c4a" value="9"/>
          <item label="10 d2010" alpha="255" color="#f69053" value="10"/>
          <item label="11 d2011" alpha="255" color="#fba45c" value="11"/>
          <item label="12 d2012" alpha="255" color="#feb467" value="12"/>
          <item label="13 d2013" alpha="255" color="#febe74" value="13"/>
          <item label="14 d2014" alpha="255" color="#fec980" value="14"/>
          <item label="15 d2015" alpha="255" color="#fed48d" value="15"/>
          <item label="16 d2016" alpha="255" color="#ffdf9a" value="16"/>
          <item label="17 d2017" alpha="255" color="#ffeaa6" value="17"/>
          <item label="18 d2018" alpha="255" color="#fff5b3" value="18"/>
          <item label="19 d2019" alpha="255" color="#ffffbf" value="19"/>
          <item label="20 d2020" alpha="255" color="#f4fbbc" value="20"/>
          <item label="21 d2021" alpha="255" color="#e9f6b8" value="21"/>
          <item label="32 Nuvem" alpha="255" color="#70dbf2" value="32"/>
          <item label="50 r2010" alpha="255" color="#c7e9ad" value="50"/>
          <item label="51 r2011" alpha="255" color="#bce4aa" value="51"/>
          <item label="52 r2012" alpha="255" color="#a3d7a6" value="52"/>
          <item label="53 r2013" alpha="255" color="#6fb3ae" value="53"/>
          <item label="54 r2014" alpha="255" color="#91cba9" value="54"/>
          <item label="55 r2015" alpha="255" color="#d3edb1" value="55"/>
          <item label="56 r2016" alpha="255" color="#80bfac" value="56"/>
          <item label="57 r2017" alpha="255" color="#4d9bb4" value="57"/>
          <item label="58 r2018" alpha="255" color="#72bbb2" value="58"/>
          <item label="59 r2019" alpha="255" color="#5ea7b1" value="59"/>
          <item label="60 r2020" alpha="255" color="#3c8fb7" value="60"/>
          <item label="61 r2021" alpha="255" color="#2b83ba" value="61"/>
          <item label="91 Hidrografia" alpha="255" color="#2543dc" value="91"/>
          <item label="100 Floresta" alpha="255" color="#0b6952" value="100"/>
          <item label="101 Nao Floresta" alpha="255" color="#e853f5" value="101"/>
          <item label="102 Nao Floresta2" alpha="255" color="#000000" value="102"/>
          <rampLegendSettings prefix="" suffix="" useContinuousLegend="0" orientation="2" minimumLabel="" direction="0" maximumLabel="">
            <numericFormat id="basic">
              <Option type="Map">
                <Option value="" type="QChar" name="decimal_separator"/>
                <Option value="6" type="int" name="decimals"/>
                <Option value="0" type="int" name="rounding_type"/>
                <Option value="false" type="bool" name="show_plus"/>
                <Option value="true" type="bool" name="show_thousand_separator"/>
                <Option value="false" type="bool" name="show_trailing_zeros"/>
                <Option value="" type="QChar" name="thousand_separator"/>
              </Option>
            </numericFormat>
          </rampLegendSettings>
        </colorrampshader>
      </rastershader>
    </rasterrenderer>
    <brightnesscontrast contrast="0" brightness="0" gamma="1"/>
    <huesaturation colorizeRed="255" colorizeBlue="128" saturation="0" colorizeStrength="100" invertColors="0" grayscaleMode="0" colorizeOn="0" colorizeGreen="128"/>
    <rasterresampler maxOversampling="2"/>
    <resamplingStage>resamplingFilter</resamplingStage>
  </pipe>
  <blendMode>0</blendMode>
</qgis>
