compile:
	$(MAKE) -C ./src/ compile

tests: compile
	python test_script.py

clean:
	$(MAKE) -C ./src/ clean
