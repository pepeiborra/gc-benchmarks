all: plot.svg

25K.txt: Pusher
	./Pusher +RTS -s -RTS 25000 2>> 25K.txt

50K.txt: Pusher
	./Pusher +RTS -s -RTS 50000 2>> 50K.txt

100K.txt: Pusher
	./Pusher +RTS -s -RTS 100000 2>> 100K.txt

200K.txt: Pusher
	./Pusher +RTS -s -RTS 200000 2>> 200K.txt

400K.txt: Pusher
	./Pusher +RTS -s -RTS 400000 2>> 400K.txt

800K.txt: Pusher
	./Pusher +RTS -s -RTS 800000 2>> 800K.txt

1.6M.txt: Pusher
	./Pusher +RTS -s -RTS 1600000 2>> 1.6M.txt

3.2M.txt: Pusher
	./Pusher +RTS -s -RTS 3200000 2>> 3.2M.txt

25Kxn.txt: Pusher
	./Pusher +RTS -s -xn -RTS 25000 2>> 25Kxn.txt

50Kxn.txt: Pusher
	./Pusher +RTS -s -xn -RTS 50000 2>> 50Kxn.txt

100Kxn.txt: Pusher
	./Pusher +RTS -s -xn -RTS 100000 2>> 100Kxn.txt

200Kxn.txt: Pusher
	./Pusher +RTS -s -xn -RTS 200000 2>> 200Kxn.txt

400Kxn.txt: Pusher
	./Pusher +RTS -s -xn -RTS 400000 2>> 400Kxn.txt

800Kxn.txt: Pusher
	./Pusher +RTS -s -xn -RTS 800000 2>> 800Kxn.txt

1.6Mxn.txt: Pusher
	./Pusher +RTS -s -xn -RTS 1600000 2>> 1.6Mxn.txt

3.2Mxn.txt: Pusher
	./Pusher +RTS -s -xn -RTS 3200000 2>> 3.2Mxn.txt

%.pause: %.txt parse
	./parse < $< > $@

noxn.dataset: 25K.pause 50K.pause 100K.pause 200K.pause 400K.pause 800K.pause 1.6M.pause 3.2M.pause
	cat 25K.pause 50K.pause 100K.pause 200K.pause 400K.pause 800K.pause 1.6M.pause 3.2M.pause > noxn.dataset

xn.dataset: 25Kxn.pause 50Kxn.pause 100Kxn.pause 200Kxn.pause 400Kxn.pause 800Kxn.pause 1.6Mxn.pause 3.2Mxn.pause
	cat 25Kxn.pause 50Kxn.pause 100Kxn.pause 200Kxn.pause 400Kxn.pause 800Kxn.pause 1.6Mxn.pause 3.2Mxn.pause > xn.dataset

%.plot: %.dataset sizes.txt
	paste sizes.txt $< > $@

plot.svg: xn.plot noxn.plot
	graph -T svg -C xn.plot -C noxn.plot -Y "Max Gen1 pause (s)" -X "Map size (K)" > plot.svg

Pusher: Pusher.hs
	ghc -O2 -threaded Pusher.hs

parse: parse.hs
	ghc -O2 parse.hs

clean:
	rm -f Pusher Pusher.o Pusher.hi 25K.txt 50K.txt 100K.txt 200K.txt 400K.txt 800K.txt 1.6M.txt 3.2M.txt 25Kxn.txt 50Kxn.txt 100Kxn.txt 200Kxn.txt 400Kxn.txt 800Kxn.txt 1.6Mxn.txt 3.2Mxn.txt 25K.pause 50K.pause 100K.pause 200K.pause 400K.pause 800K.pause 1.6M.pause 3.2M.pause 25Kxn.pause 50Kxn.pause 100Kxn.pause 200Kxn.pause 400Kxn.pause 800Kxn.pause 1.6Mxn.pause 3.2Mxn.pause xn.dataset noxn.dataset xn.plot noxn.plot plot.svg parse

install:
	cp plot.svg ${out}
