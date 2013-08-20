var container, scene, camera, renderer, controls, stats;
var clock = new THREE.Clock();
var globe, superTexture;
var radius = 100;
var particleGroup, particleAttributes;


function webglGlobeInit() {
    init();
}
function animateScene() {
    var cameraPos = {x: camera.position.x, y: camera.position.y, z: camera.position.z};
    var newCameraPos = {x: 50, y: 100, z: 250};
    var tweenToMove = new TWEEN.Tween(cameraPos).to(newCameraPos, 3500);
    tweenToMove.onUpdate(function() {
        camera.position.x = cameraPos.x;
        camera.position.y = cameraPos.y;
        camera.position.z = cameraPos.z;
    })
    tweenToMove.start();
}

function createScene() {
    scene = new THREE.Scene();
    scene.fog = new THREE.FogExp2(0x000000, 0.0001);
    var SCREEN_WIDTH = window.innerWidth, SCREEN_HEIGHT = window.innerHeight;
    var VIEW_ANGLE = 45, ASPECT = SCREEN_WIDTH / SCREEN_HEIGHT, NEAR = 0.1, FAR = 10000;
    camera = new THREE.PerspectiveCamera( VIEW_ANGLE, ASPECT, NEAR, FAR);
    scene.add(camera);
    camera.position.set(-100,150,650);
    camera.lookAt(scene.position);
    var dirLight;
    dirLight = new THREE.DirectionalLight(0xffffff);
    dirLight.position.set(-12, 30, 12).normalize();
    scene.add(dirLight);
    var skyBoxGeometry = new THREE.CubeGeometry(10000, 10000, 10000);
    var skyBoxMaterial = new THREE.MeshBasicMaterial( { color: 0x000011, side: THREE.BackSide } );
    var skyBox = new THREE.Mesh(skyBoxGeometry, skyBoxMaterial);
    scene.add(skyBox);
    addGlobe();
    addStars();
    renderer = new THREE.WebGLRenderer( {antialias:true} );
    renderer.setSize(SCREEN_WIDTH, SCREEN_HEIGHT);
    renderer.autoClear = false;
    container.appendChild(renderer.domElement);
    THREEx.WindowResize(renderer, camera);
    THREEx.FullScreen.bindKey({ charCode : 'f'.charCodeAt(0) });

    controls = new THREE.OrbitControls(camera);
    controls.maxDistance = 700;
    controls.minDistance = 200;
    controls.minPolarAngle = Math.PI * .2;
    controls.maxPolarAngle = Math.PI * .8;

}

function init() {
    if ( ! Detector.webgl )
        Detector.addGetWebGLMessage();
    container = document.getElementById('output');
    document.body.appendChild(container);
    var loader = new THREE.SceneLoader();
    loader.addGeometryHandler( "binary", THREE.BinaryLoader );
    loader.addHierarchyHandler( "obj", THREE.OBJLoader );
    loader.addHierarchyHandler( "dae", THREE.ColladaLoader );
    loader.addHierarchyHandler( "utf8", THREE.UTF8Loader );
    loader.load( "/static/js/texture.js",  function ( result ) {
        alert("Ok");
        superTexture = result.materials.superTexture;
        var progressbar = document.getElementById("progressbar");
        progressbar.style.display = "none";
        container.style.display = "block";
        createScene();
        superTexture.needsUpdate = true;
        /*superTexture.color =  0xffffff;
        superTexture.specular = 0xffffff;
        superTexture.emissive = 0x888888;*/
        animate();
        animateScene();
    } );
    stats = new Stats();
    stats.domElement.style.position = 'absolute';
    stats.domElement.style.bottom = '0px';
    stats.domElement.style.zIndex = 100;
    container.appendChild(stats.domElement);
}
function addGlobe() {
    var globRadius = radius;

    var globeGeometry = new THREE.SphereGeometry(globRadius, 128, 128);
    globe = new THREE.Mesh(globeGeometry, superTexture);
    scene.add(globe);

   /*  Здесь использовать шейдеры webGL
    var cloudMaterial = new THREE.MeshNormalMaterial({color: 0xffffff, lightMap: THREE.ImageUtils.loadTexture( "/static/images/clouds-8k.jpg" ), transparent: true});
    cloudMaterial.blending = THREE.NormalBlending;
    cloudMaterial.blendSrc = THREE.SrcAlphaFactor;
    cloudMaterial.blendDst = THREE.SrcColorFactor;
    cloudMaterial.blendEquation = THREE.AddEquation;
    var cloudSphere = new THREE.Mesh( globeGeometry.clone(), cloudMaterial );
    cloudSphere.scale.x = cloudSphere.scale.y = cloudSphere.scale.z = 1.01;
    scene.add(cloudSphere);
    */
    var particleTexture = THREE.ImageUtils.loadTexture( '/static/images/spark.png' );
    particleGroup = new THREE.Object3D();
    particleGroup.dynamic = true;
    particleAttributes = { startSize: [], startPosition: [], randomness: [] };
    particleGroup.position.y = globe.position.y;
    particleGroup.position.x = globe.position.x;
    scene.add(particleGroup);
}

function addStars() {
    var i, r = radius, starsGeometry = [ new THREE.Geometry(), new THREE.Geometry() ];
    for ( i = 0; i < 350; i ++ ) {
        var vertex = new THREE.Vector3();
        vertex.x = Math.random() * 2 - 1;
        vertex.y = Math.random() * 2 - 1;
        vertex.z = Math.random() * 2 - 1;
        vertex.multiplyScalar( r );
        starsGeometry[ 0 ].vertices.push(vertex);
    }
    for ( i = 0; i < 1500; i ++ ) {
        var vertex = new THREE.Vector3();
        vertex.x = Math.random() * 2 - 1;
        vertex.y = Math.random() * 2 - 1;
        vertex.z = Math.random() * 2 - 1;
        vertex.multiplyScalar( r );
        starsGeometry[ 1 ].vertices.push(vertex);
    }
    var stars;
    var starsMaterials = [
        new THREE.ParticleBasicMaterial( { color: 0x555555, size: 2, sizeAttenuation: false } ),
        new THREE.ParticleBasicMaterial( { color: 0xaaaaaa, size: 1, sizeAttenuation: false } ),
        new THREE.ParticleBasicMaterial( { color: 0xffffff, size: 3, sizeAttenuation: false } ),
        new THREE.ParticleBasicMaterial( { color: 0x3a3a3a, size: 1, sizeAttenuation: false } ),
        new THREE.ParticleBasicMaterial( { color: 0x1a1a1a, size: 2, sizeAttenuation: false } ),
        new THREE.ParticleBasicMaterial( { color: 0xffffff, size: 1, sizeAttenuation: false } )
    ];
    for ( i = 10; i < 30; i ++ ) {
        stars = new THREE.ParticleSystem( starsGeometry[ i % 2 ], starsMaterials[ i % 6 ] );
        stars.rotation.x = Math.random() * 6;
        stars.rotation.y = Math.random() * 6;
        stars.rotation.z = Math.random() * 6;
        s = i * 10;
        stars.scale.set( s, s, s );
        stars.matrixAutoUpdate = false;
        stars.updateMatrix();
        scene.add( stars );
    }
}
function animate() {
    requestAnimationFrame(animate);
    render();
    TWEEN.update();
}

function render() {
    //clouds.rotation += 0.0004;
    var time = 4 * clock.getElapsedTime();
    globe.rotation.y += 0.0003;
    particleGroup.rotation.y += 0.0003;
    controls.update();
    stats.update();
    renderer.render(scene, camera);
}


function addTorus(lat, long) {
    if(globe){
        var geometry = new THREE.TorusGeometry(2,1,4,30);
        var material = new THREE.MeshBasicMaterial({color:0x22aaff, transparent: true, opacity: 0.9});
        var torus = new THREE.Mesh(geometry, material);
        var coord = getCoordinate(lat, long);
        torus.position.set(coord.tx,coord.ty, coord.tz);
        torus.position.setLength(radius + 0.1);
        torus.lookAt(globe.position);
        torus.material.blending = THREE.NormalBlending;
        material.blendSrc = THREE.SrcAlphaFactor;
        material.blendDst = THREE.SrcColorFactor;
        material.blendEquation = THREE.AddEquation;
        particleGroup.add(torus);
        animateTorus(torus);
    }
}

function animateTorus(torus) {
    currentParam = {r: torus.material.color.r, g: torus.material.color.g, b: torus.material.color.b,
        radius: torus.geometry.radius, scaleX: torus.scale.x, scaleY: torus.scale.y, opacity: torus.material.opacity};
    newParam = {r: 0.3, g: 0.4, b: 0.3, scaleX:3, scaleY:3, opacity: 0.15};
    torus.geometry.radius.needsUpdate = true;
    var tween = new TWEEN.Tween(currentParam).to(newParam);
    tween.onUpdate( function() {
        console.log(torus.material.opacity);
        torus.material.color.r = currentParam.r;
        torus.material.color.g = currentParam.g;
        torus.material.color.b = currentParam.b;
        torus.scale.x = currentParam.scaleX;
        torus.scale.y = currentParam.scaleY;
        torus.material.opacity = currentParam.opacity;
    });
    tween.onComplete( function() {
        particleGroup.remove(torus);
    });
    tween.start();
}
function getCoordinate(latitude, longitude) {
    latitude = latitude * Math.PI / 180;
    longitude = -longitude * Math.PI / 180;
    var coordinate = {"tx":radius * Math.cos(latitude) * Math.cos(longitude),
        "ty":radius * Math.sin(latitude),
        "tz":radius * Math.cos(latitude) * Math.sin(longitude)};
    return coordinate;
}


/*
setInterval( function() {
     for (i = 0; i < 7; i++){
        addTorus( Math.random() * 180 - 90, Math.random() * 360 - 180);
     };
 }, 1500) */