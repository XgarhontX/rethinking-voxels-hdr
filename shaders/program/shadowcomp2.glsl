#include "/lib/common.glsl"
#ifdef FRAGMENT_SHADER

#include "/lib/vx/voxelReading.glsl"

#include "/lib/materials/specificMaterials/translucents/interactiveWaterConsts.glsl"

void main() {
    #ifdef INTERACTIVE_WATER
    vec2 newFragCoord = mod(gl_FragCoord.xy, vec2(0.5 * shadowMapResolution));
    int lodIndex = int(gl_FragCoord.x > 0.5 * shadowMapResolution) +
               2 * int(gl_FragCoord.y > 0.5 * shadowMapResolution);
    ivec3 floorCamPos = cameraPositionInt;
    ivec3 camOffset = cameraPositionInt - previousCameraPositionInt;
    if (cameraPositionInt.y == -98257195) {
        floorCamPos = ivec3(floor(cameraPosition) + 0.5 * sign(cameraPosition));
        camOffset = ivec3(1.1 * (floorCamPos - floor(previousCameraPosition)));
    }

    vec3 pos0 = vec3((newFragCoord.xy - 0.25 * shadowMapResolution), 30).xzy;
    int waterHeight = -1000;
    bool hadAnyBlocks = false;
    float attenuationCoeff = vec4(0.994, 0.983, 0.95, 0.9)[lodIndex];
    if (all(greaterThan(pos0.xz, -0.5 * voxelVolumeSize.xz + 5)) && all(lessThan(pos0.xz, 0.5 * voxelVolumeSize.xz - 5))) {
        for (int k = voxelVolumeSize.y - 1; k >= 0; k--) {
            ivec3 coords = ivec3(pos0.xz + voxelVolumeSize.xz/2, k).xzy;
            int waterData = imageLoad(voxelCols, coords * ivec3(1, 2, 1)).r >> 26;
            if ((waterData & 1) == 1) {
                waterHeight = coords.y - voxelVolumeSize.y/2;
                pos0.y = waterHeight + 0.5;
                hadAnyBlocks = true;
                break;
            }
            if (!hadAnyBlocks && (imageLoad(occupancyVolume, coords).r & 1) != 0) {
                hadAnyBlocks = true;
            }
        }
    } else {
        attenuationCoeff = attenuationCoeff * 2.0 - 1.0;
    }
    if (!hadAnyBlocks) {
        waterHeight = WATER_DEFAULT_HEIGHT - 1 - floorCamPos.y;
    }
    ivec2 prevCoords = ivec2(gl_FragCoord.xy) + camOffset.xz;
    vec4 data = vec4(0, 0, 0, waterHeight);

    if (waterHeight >= -500) {
        mat4 aroundData = mat4(
            texelFetchOffset(shadowcolor2, prevCoords, 0, ivec2(-1, 0)),
            texelFetchOffset(shadowcolor2, prevCoords, 0, ivec2( 1, 0)),
            texelFetchOffset(shadowcolor2, prevCoords, 0, ivec2( 0,-1)),
            texelFetchOffset(shadowcolor2, prevCoords, 0, ivec2( 0, 1))
        );
        ivec4 aroundHeights = ivec4(transpose(aroundData)[3] + 1000.5) - 1000 - camOffset.y;
        for (int i = 0; i < 4; i++) {
            if (aroundHeights[i] != waterHeight) {
                aroundData[i].xyz = vec3(0);
            }
        }
        for (int i = 0; i < 3; i++) {
            for (int j = 0; j < 2; j++) {
                data[i] += pow2(waveDirs[i][j])
                         * aroundData[2*j+int(waveDirs[i][j] < 0.0)][i];
            }
        }
        vec3 wind = pow2(texture(shadowcolor3, mod((pos0.xz + floorCamPos.xz%(5*1024)) * 0.2, vec2(1024))/1024.0).xyz);
        data.xyz += 0.01 * WATER_BUMP_INTERACTIVE * wind;
        data.xyz *= attenuationCoeff;
    }
    #else
        vec4 data = vec4(0);
    #endif

    /* RENDERTARGETS:2 */
    gl_FragData[0] = data;
}

#endif

#ifdef VERTEX_SHADER
void main() {
    #ifdef INTERACTIVE_WATER
        gl_Position = ftransform();
    #else
        gl_Position = vec4(-1);
    #endif
}
#endif