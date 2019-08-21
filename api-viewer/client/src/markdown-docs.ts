const REQUEST_ATTRIBUTES_DOC = `
# Request Attributes

In this documentation API requests are annotated with specific attributes as a shorthand to explain
specific requirements or behaviours. The different attributes are described below.

#### \`Safe\`

The operations is safe (ie readonly) and doesn't alter the server side state.

#### \`Idempotent\`

The operation is always [idempotent](https://en.wikipedia.org/wiki/Idempotence). This means that
the operation can be safely retried if a communications error occurred.

#### \`IdempotentWithKey\`

The operation is not intrinsically idempotent, but can be made so if the caller generates a
unique key for the request and passes it in the \`Idempotency-Key\` http header, ie:

\`\`\`
Idempotency-Key: <request-unique-key>
\`\`\`

On communication failure the caller can safely retry the request, passing the original header. A
[stripe blog post](https://stripe.com/blog/idempotency) describes this technique in detail.

#### \`JwtAuthHeader\`

A request with this annotation requires an HTTP \`Authorization\` header in bearer format.
The token itself must be a valid JWT. For example:

\`\`\`
Authorization: Bearer <jwt>
\`\`\`
`;

export const REQUEST_ATTRIBUTES = "request_attributes";

function createDocs(): { [key: string]: string } {
  const docs: { [key: string]: string } = {};
  docs[REQUEST_ATTRIBUTES] = REQUEST_ATTRIBUTES_DOC;
  return docs;
}

export const MARKDOWN_DOCS = createDocs();
