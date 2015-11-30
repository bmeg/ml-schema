FROM bgruening/docker-ipython-notebook:15.10
RUN apt-get update && apt-get install -y git autoconf libtool
RUN git clone https://github.com/google/protobuf.git && cd protobuf && ./autogen.sh && \
 ./configure && make && make install && cd python && python setup.py install
RUN git clone https://github.com/kellrott/ml-schema.git && cp -r ml-schema/ml_schema /home/ipython/py/