# coderpad-java-api
[![Circle CI](https://circleci.com/gh/skidder/coderpad-java-api.svg?style=svg&circle-token=63342339d98aaad16a0ecce990ee8f90e1268658)](https://circleci.com/gh/skidder/coderpad-java-api)

Unofficial Java client API for the [Coderpad](https://coderpad.io/) service

## Usage

### Create a Client Instance

```java
final CoderpadClient client = new CoderpadClient("secret_auth_token");
```

### Create a Pad

```java
final PadRequest request = new PadRequest();
request.setLanguage(PadLanguage.java);
request.setTitle("John Smith Interview");

try {
  final PadResponse pad = client.createPad(request);
  System.out.println("Pad created successfully, id=" + pad.getId());
} catch (CoderpadException e) {
  System.err.println("Error creating pad: " + e.getMessage());
}
```

### Update an Existing Pad

```java
final PadRequest request = new PadRequest();
request.setLanguage(PadLanguage.ruby);
request.setTitle("John Q. Smith Interview");

try {
  client.updatePad("pad-id-returned-during-create", request);
  System.out.println("Pad updated successfully");
} catch (CoderpadException e) {
  System.err.println("Error updating pad: " + e.getMessage());
}
```

### Get a Pad
```java
final PadResponse pad = client.getPad("pad-id-returned-during-create");
```

### Delete a Pad
```java
client.deletePad("pad-id-returned-during-create");
```

### List Pads
```java
final ListPadsResponse pads = client.listPads();
```

or

```java
final ListPadsResponse pads = client.listPads(ListPadsSortingTerm.created_at, ListPadsSortingOrder.desc);
```
