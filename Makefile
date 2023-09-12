JQ=jq

test: test-jq test-jqjq

.PHONY: test-jq
test-jq:
	sed '/SKIP_JQ/q' jqjq.test | "${JQ}" --run-tests

.PHONY: test-jqjq
test-jqjq:
	./jqjq --jq "${JQ}" --run-tests < jqjq.test
