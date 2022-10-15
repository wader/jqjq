JQ=jq

test: test-jq test-jqjq

.PHONY: test-jq
test-jq:
	cat jqjq.test | sed '/SKIP_JQ/q' | jq --run-tests

.PHONY: test-jqjq
test-jqjq:
	./jqjq --jq "${JQ}" --run-tests < jqjq.test
