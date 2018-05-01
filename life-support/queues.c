/* -*- Mode: C; Tab-Width: 4 -*- */

/* Queue manipulation routines */

#include "std.h"

#include "life_types.h"
#include "embed.h"
#include "life_prototypes.h"


/* Create a queue */

EmbPtr CreateQueue (int nElements, int elementSize)
{
  EmbPtr cp = EmbCommAreaAlloc (sizeof (EmbQueue) + (nElements * elementSize));
  register EmbQueue* q = (EmbQueue*) HostPointer (cp);

	q->element_size = elementSize;
	q->queue_size = nElements;
	q->put_index = 0;
	q->take_index = 0;
	q->signal = -1;

	return (cp);
}


/* Number of free elements */

int EmbQueueSpace (EmbQueue* qp)
{ 
  register EmbWord put = qp->put_index;
  register EmbWord take = qp->take_index;

	if (take > put)
		return (take - put - 1);
	else
		 return (take - put - 1 + qp->queue_size);
}


/* Number of non-free elements */

int EmbQueueFilled (EmbQueue* qp)
{ 
  register EmbWord put = qp->put_index;
  register EmbWord take = qp->take_index;

	if (put >= take)
		return (put - take);
	else
		return (put - take + qp->queue_size);
}


/* Put element into queue */

void EmbQueuePut (EmbQueue* qp_arg, PtrV ep)
{
  register EmbQueue* qp = qp_arg;
  register EmbWord put = qp->put_index;
  EmbWord original_put = put;
  byte* element_array = (byte*)(&qp->first_element);

	/* Fill in the element at the put index */
	memcpy (&element_array[put * qp->element_size], ep, (size_t) qp->element_size);

	/* Compute the incremented put index */
	put++;
	if (put >= qp->queue_size)
		put = 0;

	/* Wait if queue is full -- Queue sizes will be chosen so that this is rare */
	while (put == qp->take_index);

	/* Store the incremented put index into the queue header */
	qp->put_index = put;

	/* If the queue had been empty, send a signal to the taker */
	if (original_put == qp->take_index)
		EmbSendSignal (qp->signal);
}


/* Put element into word queue */

void EmbQueuePutWord (EmbQueue* qp_arg, EmbWord elt)
{ 
  register EmbQueue* qp = qp_arg;
  register EmbWord put = qp->put_index;
  EmbWord original_put = put;
  register EmbWord* element_array = (EmbWord*)(&qp->first_element);

	/* Fill in the element at the put index */
	element_array[put] = elt;
	
	/* Compute the incremented put index */
	put++;
	if (put >= qp->queue_size)
		put = 0;
	
	/* Wait if queue is full -- Queue sizes will be chosen so that this is rare */
	while (put == qp->take_index);
	
	/* Store the incremented put index into the queue header */
	qp->put_index = put;
	
	/* If the queue had been empty, send a signal to the taker */
	if (original_put == qp->take_index)
		EmbSendSignal (qp->signal);
}


/* Put element into byte queue */

void EmbQueuePutByte (EmbQueue* qp_arg, byte elt)
{
  register EmbQueue* qp = qp_arg;
  register EmbWord put = qp->put_index;
  EmbWord original_put = put;
  register byte* element_array = (byte*)(&qp->first_element);

	/* Fill in the element at the put index */
	element_array[put] = elt;
	
	/* Compute the incremented put index */
	put++;
	if (put >= qp->queue_size)
		put = 0;
	
	/* Wait if queue is full -- Queue sizes will be chosen so that this is rare */
	while (put == qp->take_index);
	
	/* Store the incremented put index into the queue header */
	qp->put_index = put;
	
	/* If the queue had been empty, send a signal to the taker */
	if (original_put == qp->take_index)
		EmbSendSignal (qp->signal);
}


/* Take element from queue */

bool EmbQueueTake (EmbQueue* qp_arg, PtrV ep)
{
  register EmbQueue* qp = qp_arg;
  register EmbWord put = qp->put_index;
  register EmbWord take = qp->take_index;
  register byte* element_array = (byte*)(&qp->first_element);

	/* Check for empty queue */
	if (put == take)
		return (FALSE);
	
	/* Copy the element at the take index */
	memcpy (ep, &element_array[take * qp->element_size], (size_t) qp->element_size);
	
	/* Increment the take index and store it back */
	take++;
	if (take >= qp->queue_size)
		take = 0;
	qp->take_index = take;

	return (TRUE);
}


/* Take element from word queue */

EmbWord EmbQueueTakeWord (EmbQueue* qp_arg)
{
  register EmbQueue* qp = qp_arg;
  register EmbWord put = qp->put_index;
  register EmbWord take = qp->take_index;
  register EmbWord* element_array = (EmbWord*)(&qp->first_element);
  register EmbWord elt;

	/* Check for empty queue--should not happen */
	if (put == take)
		return (FALSE);
	
	/* Copy the element at the take index */
	elt = element_array[take];
	
	/* Increment the take index and store it back */
	take++;
	if (take >= qp->queue_size)
		take = 0;
	qp->take_index = take;

	return (elt);
}


/* Take element from byte queue */

byte EmbQueueTakeByte (EmbQueue* qp_arg)
{
  register EmbQueue* qp = qp_arg;
  register EmbWord put = qp->put_index;
  register EmbWord take = qp->take_index;
  register byte* element_array = (byte*)(&qp->first_element);
  register byte elt;

	/* Check for empty queue--should not happen */
	if (put == take)
		return (FALSE);
	
	/* Copy the element at the take index */
	elt = element_array[take];
	
	/* Increment the take index and store it back */
	take++;
	if (take >= qp->queue_size)
		take = 0;
	qp->take_index = take;

	return (elt);
}


/*** Transferring multiple queue elements at a time ***/

/* Each of these routines returns the number of elements actually transferred */
/* None of these routines ever waits */

/* Put multiple bytes */

int EmbQueuePutBytes (EmbQueue* qp_arg, byte* buffer, int length)
{
  register EmbQueue* qp = qp_arg;
  register EmbWord put = qp->put_index;
  EmbWord original_put = put;
  register EmbWord take;
  register byte* element_array = (byte*)(&qp->first_element);
  int actual_length = 0;
  register EmbWord count;

	/* Loop transferring contiguous blocks of elements */
	while (length > 0)
	  {
		/* Fill in as many elements as we can */
		take = qp->take_index;
		if (take > put)
			count = take - put - 1;
		else if (take == 0)
			count = qp->queue_size - put - 1;
		else
			count = qp->queue_size - put;
		if (count > length)
			 count = length;
		memcpy (&element_array[put], buffer, (size_t) count);
		buffer += count;
		length -= count;
		actual_length += count;
		put += count;
		if (put == qp->queue_size)
			put = 0;
		qp->put_index = put;
	  }
	
	/* If the queue had been empty, send a signal to the taker */
	if (original_put == qp->take_index)
		EmbSendSignal (qp->signal);

	return (actual_length);
}


/* Put multiple words */

int EmbQueuePutWords (EmbQueue* qp_arg, EmbWord* buffer, int length)
{
  register EmbQueue* qp = qp_arg;
  register EmbWord put = qp->put_index;
  EmbWord original_put = put;
  register EmbWord take;
  register EmbWord* element_array = (EmbWord*)(&qp->first_element);
  int actual_length = 0;
  register EmbWord count;

	/* Loop transferring contiguous blocks of elements */
	while (length > 0)
	  { 
		/* Fill in as many elements as we can */
		take = qp->take_index;
		if (take > put)
			count = take - put - 1;
		else if (take == 0)
			count = qp->queue_size - put - 1;
		else
			count = qp->queue_size - put;
		if (count > length)
			count = length;
		memcpy (&element_array[put], buffer, (size_t) count * sizeof (EmbWord));
		buffer += count;
		length -= count;
		actual_length += count;
		put += count;
		if (put == qp->queue_size)
			put = 0;
		qp->put_index = put;
	  }
	
	/* If the queue had been empty, send a signal to the taker */
	if (original_put == qp->take_index)
		EmbSendSignal (qp->signal);

	return (actual_length);
}


/* Take multiple bytes */

int EmbQueueTakeBytes (EmbQueue* qp_arg, byte* buffer, int length)
{
  register EmbQueue* qp = qp_arg;
  register EmbWord take = qp->take_index;
  register EmbWord put;
  register byte* element_array = (byte*)(&qp->first_element);
  int actual_length = 0;
  register EmbWord count;

	/* Loop transferring contiguous blocks of elements */
	while (length > 0)
	  { 
		/* Fill in as many elements as we can */
		put = qp->put_index;
		if (put >= take)
			count = put - take;
		else
			count = qp->queue_size - take;
		if (count > length)
			count = length;
		memcpy (buffer, &element_array[take], (size_t) count);
		buffer += count;
		length -= count;
		actual_length += count;
		take += count;
		if (take == qp->queue_size)
			take = 0;
		qp->take_index = take;
	  }

	return (actual_length);
}


/* Take multiple words */

int EmbQueueTakeWords (EmbQueue* qp_arg, EmbWord* buffer, int length)
{ 
  register EmbQueue* qp = qp_arg;
  register EmbWord take = qp->take_index;
  register EmbWord put;
  register EmbWord* element_array = (EmbWord*)(&qp->first_element);
  int actual_length = 0;
  register EmbWord count;

	/* Loop transferring contiguous blocks of elements */
	while (length > 0)
	  { 
		/* Fill in as many elements as we can */
		put = qp->put_index;
		if (put >= take)
			count = put - take;
		else
			count = qp->queue_size - take;
		if (count > length)
			count = length;
		memcpy (buffer, &element_array[take], (size_t) count * sizeof (EmbWord));
		buffer += count;
		length -= count;
		actual_length += count;
		take += count;
		if (take == qp->queue_size)
			take = 0;
		qp->take_index = take;
	  }

	return (actual_length);
}


/* Reset an incoming (guest->host) queue --
      If the queue was created by the FEP or the guest, remove its signal handler */

void ResetIncomingQueue (EmbQueue* q)
{
	q->put_index = 0;
	q->take_index = 0;
	if (GuestPointer (q) > EmbCommAreaPtr->host_buffer_start + EmbCommAreaPtr->host_buffer_size)
	  {
		RemoveSignalHandler (q->signal);
		q->signal = -1;
	  }

}


/* Reset an outgoing (host->guest) queue --
      Note that the guest is no longer listening for signals from this queue */

void ResetOutgoingQueue (EmbQueue* q)
{
	q->put_index = 0;
	q->take_index = 0;
	q->signal = -1;
}
