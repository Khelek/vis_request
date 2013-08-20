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
        }
    },
    "materials" :
    {

            "superTexture" : {
                "type" : "MeshPhongMaterial",
                    "parameters": { "map" : "colors", "bumpMap": "bumpy", "bumpScale": 4, "specularMap": "shiny",
                "reflectivity": 10 }

            }
    },
    "defaults" :
    {
        "bgcolor" : [ 0, 0, 0 ],
        "bgalpha" : 1.000000,
        "camera"  : "default_camera"
    }

}