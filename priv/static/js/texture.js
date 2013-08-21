{
    "urlBaseType" : "relativeToScene",
    "textures" : {
        "bumpy": {
            "url" : "../../images/earth-bump-8k.jpg"
        },
        "shiny": {
            "url" : "../../images/earth-specular.jpg"
        },
        "colors": {
            "url" : "../../images/earth-clouds-8k.jpg"
        },
        "space": {
            "url" : "../../images/space.jpg"
        }
    },
    "materials" :
    {
            "superTexture" : {
                "type" : "MeshPhongMaterial",
                    "parameters" : { "color" : "#fff", "specular" : "#fff", "emissive": "#888", "map" : "colors", "bumpMap" : "bumpy", "bumpScale" : 4, "specularMap": "shiny",
                "reflectivity" : 10 }

            },
            "spaceTexture" : {
                "type" : "MeshBasicMaterial",
                "parameters" : { "map" : "space", "side" : "THREE.DoubleSide" }
            }
    },
    "defaults" :
    {
        "bgcolor" : [ 0, 0, 0 ],
        "bgalpha" : 1.000000,
        "camera"  : "default_camera"
    }
}