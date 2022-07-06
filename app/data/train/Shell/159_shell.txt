
./clean.sh

javac -d build/modules \
--module-source-path src \
`find src -name "*.java"`

jar --create --file=mlib/com.acme.bids.db@1.0.jar \
--module-version=1.0 -C build/modules/com.acme.bids.db . 

jar --create --file=mlib/com.acme.bids.service@1.0.jar \
--module-version=1.0 \
--main-class=com.acme.bids.service.api.UserService \
-C build/modules/com.acme.bids.service . 

jar --create --file=mlib/com.acme.bids.app@1.0.jar \
--module-version=1.0 \
--main-class=com.acme.bids.app.App \
-C build/modules/com.acme.bids.app . 

