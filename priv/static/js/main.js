var container, scene, camera, renderer, controls, stats;
var clock = new THREE.Clock();
var globe;
var radius = 100;
var particleGroup, particleAttributes;


function webglGlobeInit() {
    init();
    animate();
    animateScene();
}
function animateScene() {
    var cameraPos = {x: camera.position.x, y: camera.position.y, z: camera.position.z}
    var newCameraPos = {x: 200, y: 200, z: 100}
    var tweenToMove = new TWEEN.Tween(cameraPos).to(newCameraPos, 3500);
    tweenToMove.onUpdate(function() {
        camera.position.x = cameraPos.x;
        camera.position.y = cameraPos.y;
        camera.position.z = cameraPos.z;
    })
    tweenToMove.start();
}
function animationSprite(sprite) {
    var scaleCurrent = {x: sprite.scale.x, y: sprite.scale.y, z: sprite.scale.z,
        r : sprite.material.color.r, g: sprite.material.color.g, b: sprite.material.color.b, opacity: sprite.material.opacity};

    var scale = {x: 12, y: 12, z: 1, r: 3, g: 0, b:0};
    var tweenExpande = new TWEEN.Tween(scaleCurrent).to(scale, 900);
    tweenExpande.onUpdate(function() {
        sprite.scale.x = scaleCurrent.x;
        sprite.scale.y = scaleCurrent.y;
        sprite.scale.z = scaleCurrent.z;
        sprite.material.color.r = scaleCurrent.r;
        sprite.material.color.g = scaleCurrent.g;
        sprite.material.color.b = scaleCurrent.b;
    });

    var reverScale = {x: 1, y: 1, z: 1, r: 1, g: 1, b:3, opacity: 0};
    var tweenSqueeze = new  TWEEN.Tween(scaleCurrent).to(reverScale, 400);
    tweenSqueeze.onUpdate(function() {
        sprite.scale.x = scaleCurrent.x;
        sprite.scale.y = scaleCurrent.y;
        sprite.scale.z = scaleCurrent.z;
        sprite.material.color.r = scaleCurrent.r;
        sprite.material.color.g = scaleCurrent.g;
        sprite.material.color.b = scaleCurrent.b;
        sprite.material.opacity = scaleCurrent.opacity;
    });
    tweenExpande.chain(tweenSqueeze);
    tweenExpande.start();
    tweenSqueeze.onComplete(
    function(){
        particleGroup.remove(sprite);
    });
}


function init() {
    if ( ! Detector.webgl )
        Detector.addGetWebGLMessage();
    scene = new THREE.Scene();
    scene.fog = new THREE.FogExp2(0x000000, 0.0001);
    var SCREEN_WIDTH = window.innerWidth, SCREEN_HEIGHT = window.innerHeight;
    var VIEW_ANGLE = 45, ASPECT = SCREEN_WIDTH / SCREEN_HEIGHT, NEAR = 0.1, FAR = 10000;
    camera = new THREE.PerspectiveCamera( VIEW_ANGLE, ASPECT, NEAR, FAR);
    scene.add(camera);
    camera.position.set(-100,150,650);
    camera.lookAt(scene.position);
    renderer = new THREE.WebGLRenderer( {antialias:true} );
    renderer.setSize(SCREEN_WIDTH, SCREEN_HEIGHT);
    renderer.autoClear = false;
    container = document.createElement('div');
    document.body.appendChild(container);
    container.appendChild(renderer.domElement);
    THREEx.WindowResize(renderer, camera);
    THREEx.FullScreen.bindKey({ charCode : 'f'.charCodeAt(0) });

    controls = new THREE.OrbitControls(camera);
    controls.maxDistance = 700;
    controls.minDistance = 200;
    controls.minPolarAngle = Math.PI * .2;
    controls.maxPolarAngle = Math.PI * .8;
    stats = new Stats();
    stats.domElement.style.position = 'absolute';
    stats.domElement.style.bottom = '0px';
    stats.domElement.style.zIndex = 100;
    container.appendChild(stats.domElement);

    var dirLight;
    dirLight = new THREE.DirectionalLight(0xeeeeee);
    dirLight.position.set(-12, 6, 12).normalize();
    scene.add(dirLight);
    var skyBoxGeometry = new THREE.CubeGeometry(10000, 10000, 10000);
    var skyBoxMaterial = new THREE.MeshBasicMaterial( { color: 0x000011, side: THREE.BackSide } );
    var skyBox = new THREE.Mesh(skyBoxGeometry, skyBoxMaterial);
    scene.add(skyBox);

    addGlobe();
    addStars();

}

function addGlobe() {
    var colors = THREE.ImageUtils.loadTexture( "/static/images/earth-day.jpg" );
    var bumpy = THREE.ImageUtils.loadTexture( "/static/images/earth-topo.jpg" );
    var shiny = THREE.ImageUtils.loadTexture( "/static/images/earth-specular.jpg" );

    var superTexture = new THREE.MeshPhongMaterial( { color: 0xffffff, map: colors,
        bumpMap: bumpy, bumpScale: 4, specular: 0xffffff, specularMap: shiny, emissive: 0x888888 } );

    var globRadius = radius;
    var globeGeometry = new THREE.SphereGeometry(globRadius, 32, 32);
    globe = new THREE.Mesh(globeGeometry, superTexture);
    scene.add(globe);
    var axes = new THREE.AxisHelper(550);
    axes.position = globe.position;
    scene.add(axes);

    var particleTexture = THREE.ImageUtils.loadTexture( '/static/images/spark.png' );
    particleGroup = new THREE.Object3D();
    particleAttributes = { startSize: [], startPosition: [], randomness: [] };
    var totalParticles = 1;
    var radiusRange = globRadius + 2;
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
    var time = 4 * clock.getElapsedTime();
    globe.rotation.y += 0.003;
    particleGroup.rotation.y += 0.003;
    controls.update();
    stats.update();
    renderer.render(scene, camera);
}

var particleTexture = THREE.ImageUtils.loadTexture( '/static/images/spark.png' );

function addSprite(lat, long) {
    var spriteMaterial = new THREE.SpriteMaterial( { map: particleTexture, useScreenCoordinates: false, color: 0xffffff } );
    sprite = new THREE.Sprite( spriteMaterial );
    sprite.scale.set(1, 1, 1.0);
    var coord = getCoordinate(lat,long);
    sprite.position.set(coord.tx,coord.ty, coord.tz);
    sprite.position.setLength(radius +2);
    // sprite.color.setRGB( Math.random(),  Math.random(),  Math.random() );
    sprite.material.color.setHSL(1.0, 1.0, 2.0 );
    sprite.material.blending = THREE.AdditiveBlending; // "glowing" particles
    particleGroup.add(sprite);
    animationSprite(sprite);
}

function getCoordinate(latitude, longitude) {
    latitude = latitude * Math.PI / 180;
    longitude = -longitude * Math.PI / 180;
    var coordinate = {"tx":radius * Math.cos(latitude) * Math.cos(longitude),
        "ty":radius * Math.sin(latitude),
        "tz":radius * Math.cos(latitude) * Math.sin(longitude)};
    return coordinate;

}
/*setInterval( function() {
    for (i = 0; i < 10; i++){
        addSprite( Math.random() * 180 - 90, Math.random() * 360 - 180);
    };
}, 500)       */